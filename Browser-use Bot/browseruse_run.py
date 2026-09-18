#!/usr/bin/env python3
"""
browser-use arm - an off-the-shelf open-source agent (browser-use) driven by a standalone model API
(default Claude Opus 4.8), attempting to complete a Qualtrics survey without being flagged as a bot.

It is given the SAME prompt the commercial agents received (prompt.txt), so only
the agent *framework* differs from the commercial-agent condition. One autonomous run per invocation.
The verdict comes from your Qualtrics data (Finished==1 + which detection flags fired). After each run
it prints a one-line summary: status, step count, wall-clock time, thinking budget, and token usage/cost.

Extended thinking is OFF by default (off-the-shelf condition): browser-use only makes non-streaming
API calls, so a high thinking budget exceeds the Anthropic SDK's non-streaming max_tokens limit and
fails ("Streaming is required..."). Pass --thinking-budget N only to try a small budget.

Usage:
    python browseruse_run.py
    python browseruse_run.py --url https://<dc>.qualtrics.com/jfe/form/SV_xxxx --model claude-opus-4-8
    python browseruse_run.py --thinking-budget 0        # disable extended thinking

Needs ANTHROPIC_API_KEY in the environment, and a display (Xvfb) so the browser runs HEADED.
"""
import argparse
import asyncio
import csv
import pathlib
import time

from browser_use import Agent, ChatAnthropic

try:
    from browser_use import BrowserProfile
except Exception:  # pragma: no cover - depends on installed browser-use surface
    BrowserProfile = None

HERE = pathlib.Path(__file__).resolve().parent
SURVEY_URL = "PASTE_YOUR_QUALTRICS_URL_HERE"  # set your own survey, or pass --url
SENTINEL = "PASTE_YOUR_PROMPT_HERE"


def load_task() -> str:
    p = HERE / "prompt.txt"
    if not p.exists():
        raise SystemExit("missing prompt.txt: paste your prompt there (see README).")
    task = p.read_text(encoding="utf-8").strip()
    if not task or SENTINEL in task:
        raise SystemExit("prompt.txt still holds the placeholder: paste your prompt (see README).")
    return task


def build_llm(model: str, thinking_budget: int):
    # Extended thinking approximates the "high effort" used for the other arms. budget must be < max_tokens.
    if thinking_budget and thinking_budget > 0:
        return ChatAnthropic(
            model=model,
            max_tokens=thinking_budget + 8192,
            thinking={"type": "enabled", "budget_tokens": thinking_budget},
        )
    return ChatAnthropic(model=model)


def make_agent(task: str, llm):
    flags = ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"]

    def build(**extra):
        # calculate_cost=True enables token/$ tracking; drop it if this version doesn't accept it.
        try:
            return Agent(task=task, llm=llm, calculate_cost=True, **extra)
        except TypeError:
            return Agent(task=task, llm=llm, **extra)

    if BrowserProfile is not None:
        try:
            # Force headed + the Chromium flags a headless server/container needs to launch at all.
            return build(browser_profile=BrowserProfile(headless=False, chromium_sandbox=False, args=flags))
        except TypeError:
            try:
                return build(browser_profile=BrowserProfile(headless=False))
            except TypeError:
                pass
    print("WARNING: could not set a browser profile; relying on defaults - if the browser fails to launch, see README.")
    return build()


async def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--url", default=SURVEY_URL)
    ap.add_argument("--model", default="claude-opus-4-8")  # must match the other arms' exact model id
    ap.add_argument("--thinking-budget", type=int, default=0)  # 0 = off (default); browser-use is non-streaming, so high budgets fail
    ap.add_argument("--max-steps", type=int, default=200)
    ap.add_argument("--timeout", type=int, default=1800)
    args = ap.parse_args()

    if "qualtrics.com" not in args.url:
        raise SystemExit(f"--url must be a qualtrics.com link (got {args.url!r})")

    task = load_task().replace("{URL}", args.url)
    llm = build_llm(args.model, args.thinking_budget)
    agent = make_agent(task, llm)

    t0 = time.time()
    h = None
    try:
        h = await asyncio.wait_for(agent.run(max_steps=args.max_steps), timeout=args.timeout)
    finally:
        elapsed = round(time.time() - t0, 1)
        effort = args.thinking_budget if args.thinking_budget > 0 else "off"
        usage = getattr(h, "usage", None) if h is not None else None
        status = ("DONE" if h.is_done() else "NOT DONE") if h is not None else "NO RESULT"
        steps = len(h.history) if (h is not None and getattr(h, "history", None) is not None) else ""
        if h is not None:
            print(f"\n{status}: {h.final_result()}")
        print(f"[summary] {status}  steps={steps}  time={elapsed:.0f}s ({elapsed / 60:.1f} min)  thinking={effort}  usage={usage}")
        g = lambda a: getattr(usage, a, "") if usage is not None else ""
        csv_path = HERE / "runs.csv"
        is_new = not csv_path.exists()
        with csv_path.open("a", newline="") as fh:
            w = csv.writer(fh)
            if is_new:
                w.writerow(["run_id", "status", "steps", "duration_s", "total_cost", "total_tokens", "thinking", "model"])
            w.writerow([time.strftime("%Y%m%d-%H%M%S"), status, steps, elapsed,
                        g("total_cost"), g("total_tokens"), effort, args.model])


if __name__ == "__main__":
    asyncio.run(main())
