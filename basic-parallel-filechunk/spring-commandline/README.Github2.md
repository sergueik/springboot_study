# Preventing CI Failures from Large Files in GitHub Enterprise

Problem:
A 20MB resource file committed to the repository caused a 500 Internal Server Error on the GitHub Enterprise server with no useful diagnostics.
A temporary fix (removing the file and reducing fetch-depth) resolved the issue only by accident and does not prevent recurrence.

Sad joke (optional, but true):
"I felt like a hero for a moment — then I realized I had only defeated the symptom, not the villain."

Root cause:
Git repositories are not designed to store large binary artifacts in normal history. Large blobs can break clone, fetch, and CI operations, especially on enterprise servers with proxies and storage limits.

Permanent solution options (layered defense):

1. Use Git LFS for large files (official best practice)
   Track and store large or binary files outside normal Git history.

   Example:
   git lfs track "*.zip"
   git lfs track "*.jar"
   git lfs track "*.pdf"
   git lfs track "*.bin"
   git add .gitattributes
   git commit -m "Track large files with Git LFS"

2. Enforce file size limits with server-side hooks (enterprise-level control)
   Configure a pre-receive hook on GitHub Enterprise Server to reject commits that contain files over a defined size (e.g., 10MB).
   This prevents large files from ever entering repository history and provides a clear error message to developers.

3. Add a CI guardrail to block large files in pull requests
   Example GitHub Actions step:

   - name: Fail if large files are committed
     run: |
       git diff --name-only origin/main...HEAD | while read f; do
         if [ -f "$f" ]; then
           size=$(stat -c%s "$f")
           if [ "$size" -gt 10485760 ]; then
             echo "ERROR: File $f exceeds 10MB limit"
             exit 1
           fi
         fi
       done

4. Define repository policy and documentation
   Example policy statement:
   "Large binary files must be stored in Git LFS. Commits introducing files larger than 10MB are rejected by server hooks and CI checks. Workflow fetch-depth is not considered a mitigation for repository size violations."

5. Clean up existing history if needed
   If a large file already entered history, remove it using history rewrite tools (e.g., git filter-repo) and force-push after team coordination.

Key point:
Reducing fetch-depth or removing the file after the fact is a workaround, not a solution.
Prevention must happen at push time (Git LFS, hooks) and at review time (CI checks).

Goal:
Turn a mysterious "500 Internal Server Error" into a clear, actionable message:
"Push rejected: file exceeds 10MB limit. Use Git LFS."

----

# Preventing CI Failures from Large Files in GitHub Enterprise

Problem:
A 20MB resource file committed to the repository caused a 500 Internal Server Error on the GitHub Enterprise server with no useful diagnostics.
A temporary fix (removing the file and reducing fetch-depth) resolved the issue only by accident and does not prevent recurrence.

Sad joke (optional, but true):
"I felt like a hero for a moment — then I realized I had only defeated the symptom, not the villain."

Root cause:
Git repositories are not designed to store large binary artifacts in normal history. Large blobs can break clone, fetch, and CI operations, especially on enterprise servers with proxies and storage limits.

Permanent solution options (layered defense):

1. Use Git LFS for large files (official best practice)
   Track and store large or binary files outside normal Git history.

   Example:
   git lfs track "*.zip"
   git lfs track "*.jar"
   git lfs track "*.pdf"
   git lfs track "*.bin"
   git add .gitattributes
   git commit -m "Track large files with Git LFS"

2. Enforce file size limits with server-side hooks (enterprise-level control)
   Configure a pre-receive hook on GitHub Enterprise Server to reject commits that contain files over a defined size (e.g., 10MB).
   This prevents large files from ever entering repository history and provides a clear error message to developers.

3. Add a CI guardrail to block large files in pull requests
   Example GitHub Actions step:

   - name: Fail if large files are committed
     run: |
       git diff --name-only origin/main...HEAD | while read f; do
         if [ -f "$f" ]; then
           size=$(stat -c%s "$f")
           if [ "$size" -gt 10485760 ]; then
             echo "ERROR: File $f exceeds 10MB limit"
             exit 1
           fi
         fi
       done

4. Define repository policy and documentation
   Example policy statement:
   "Large binary files must be stored in Git LFS. Commits introducing files larger than 10MB are rejected by server hooks and CI checks. Workflow fetch-depth is not considered a mitigation for repository size violations."

5. Clean up existing history if needed (unfortunate but sometimes necessary)
   If a large file already entered history, the damaged repository may require a history rewrite using tools like `git filter-repo` or `BFG Repo-Cleaner`.
   Coordination and careful communication are critical, as this rewrites commits and requires force-pushes to shared branches.

Key point:
Reducing fetch-depth or removing the file after the fact is a workaround, not a solution.
Prevention must happen at push time (Git LFS, hooks) and at review time (CI checks).

Goal:
Turn a mysterious "500 Internal Server Error" into a clear, actionable message:
"Push rejected: file exceeds 10MB limit. Use Git LFS."

Optional “spicy senior-level vocabulary” note:
Your periodic attempts at joking in the face of executives can be playfully branded as **“Spicy Senior-Level Vocabulary”** — a recurring, sanctioned injection of dry humor into otherwise stressful operational discussions.
