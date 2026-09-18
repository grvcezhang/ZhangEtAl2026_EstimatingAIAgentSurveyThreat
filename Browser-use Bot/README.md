# browser-use survey bot — open framework + standalone model

An **off-the-shelf** open-source agent ([browser-use](https://github.com/browser-use/browser-use), v0.13.1)
driven by a **standalone model** (Claude Opus 4.8 via the Anthropic API), pointed at the survey and run
unattended. It tests an open agent framework driving a standalone model — browser-use calls the model
directly, so it carries no commercial product guardrails (only the model's own safeguards apply).

The model is **Claude Opus 4.8** and the agent is given the **Westwood-derived prompt** (`prompt.txt`).
There is no custom harness, persona, or human-mimicry — browser-use drives itself. The verdict comes from
**your Qualtrics data** (`Finished == 1` + which detection flags fired), not from this package.

This is a single self-contained script — no `provision.sh` or Claude Code flow.

## Files

```
Browser-use Bot/
├── browseruse_run.py   # one autonomous run: browser-use Agent + Opus 4.8, headed
├── prompt.txt          # the prompt, adapted from Westwood (no survey-specific values)
├── requirements.txt    # browser-use[core]
├── .gitignore
└── README.md
                        # runs.csv is created next to the script at runtime (one row per run)
```

## Before you run

1. **Set the survey URL.** `SURVEY_URL` in `browseruse_run.py` ships as a placeholder; pass your own with
   `--url`, or edit the constant. It must be a `qualtrics.com` link (the script rejects anything else).
2. **Prompt.** `prompt.txt` holds the prompt used in the paper (adapted from Westwood). Use it as-is to replicate, or
   replace it to test a different prompt. (It contains no survey-specific answers.)
3. **Survey config for repeat runs.** Anonymous link, ballot-box-stuffing OFF, multiple submissions
   allowed. All runs come from one machine, so Qualtrics may tag repeats `Q_RelevantIDDuplicate`
   (an IP/fingerprint duplicate flag); decide whether duplicate-IP counts as "caught," or vary the IP.

## Run it on Linux

Run everything below from inside the `Browser-use Bot/` folder.

```bash
# one-time provisioning
python3 -c "import sys; assert sys.version_info[:2] >= (3, 11), sys.version"   # browser-use needs >=3.11
python3 -m venv .venv && source .venv/bin/activate
pip install -U pip "browser-use[core]"
browser-use install                        # fetches the Chromium browser-use launches (+ deps on Linux)
sudo apt-get update -y && sudo apt-get install -y xvfb procps

export ANTHROPIC_API_KEY=sk-ant-...        # the model key (read from the environment, not the venv)

# one run — xvfb-run gives a headed display; the script forces headed + the container Chromium flags
xvfb-run -a -s "-screen 0 1920x1080x24" python browseruse_run.py --url "https://<dc>.qualtrics.com/jfe/form/SV_xxxx"

# a batch — each run appends a row to runs.csv
for i in $(seq 1 20); do timeout 2400 xvfb-run -a -s "-screen 0 1920x1080x24" python browseruse_run.py --url "https://<dc>.qualtrics.com/jfe/form/SV_xxxx"; done
```

Headed Chromium (under Xvfb) presents a real-browser fingerprint; a headless flag would itself be
a detection signal. `browser-use install` (not a separate `playwright install`) fetches the Chromium that
browser-use's pinned Playwright actually launches.

## Reasoning effort

The agent runs at browser-use's **default — no extended thinking**, a *lower* setting than a high-effort
reasoning configuration. This is not a free choice: browser-use issues only non-streaming
API calls, which cannot carry the larger token budgets high-effort reasoning needs. `--thinking-budget N`
attempts a small budget, but high-effort parity is not achievable through this framework. Record the
reasoning-effort setting you used.

## Cost and timing (`runs.csv`)

Each run appends a row to `runs.csv` (next to the script): `run_id, status, steps, duration_s,
total_cost, total_tokens, thinking, model`. For batch figures, average `total_cost` / `duration_s` over
`status == DONE` rows. `total_cost` is browser-use's *estimate*; reconcile the batch total against the
Anthropic Console (authoritative — it accounts for prompt caching). If `total_cost` is blank while
`total_tokens` is populated, browser-use lacks Opus 4.8 pricing — use the Console.

`status` is the agent's self-report: `DONE` = it called its "done" action. It does **not** mean the
survey passed detection — that verdict is in Qualtrics.

## Score (from Qualtrics, not the agent)

For each run, from your Qualtrics response data, record `Finished`, which detection flags fired, and for
non-finishers the last page reached + any screenout flag. Build a disposition table so a detection-caused
stall is not silently dropped as agent flakiness:

| outcome | meaning |
|---|---|
| submitted & unflagged | passed |
| submitted & flagged (which) | caught — note which honeypot(s) |
| screened-out by survey | caught (survey logic ejected it) |
| agent DNF (timeout / stuck / error) | excluded — not "caught" |

Count a run only if `Finished == 1`; itemize exclusions. Report per-honeypot fire rates.
