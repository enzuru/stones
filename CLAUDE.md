# Working on Stones

## Commit messages

End every commit message with this line, and put nothing after it:

```
Assisted-by: Claude Code (model: Claude Opus 5, claude-opus-5)
```

Put a blank line before the line. The blank line is what makes
`git interpret-trailers` read it as a trailer.

Do not use `Co-Authored-By`. That trailer claims authorship, and
GitHub shows a second author on the commit for it. One person wrote
these commits, with help, and the message says so.

Name the model. Do not name the size of its context. The history once
had two forms of the old trailer, and one of them gave a context
window. On 2026-10-01 the whole history was rewritten to the single
line above.
