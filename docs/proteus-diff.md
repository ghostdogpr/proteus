# proteus-diff

**proteus-diff** is a command-line tool that detects and classifies changes between two `.proto` schemas. It tells you whether a schema change is safe, potentially breaking, or definitely breaking — useful for CI checks, code reviews, or local validation before committing.

It's distributed as a **standalone native binary** — no JVM required. If you work with Protobuf, you can use it regardless of your stack.

::: tip
proteus-diff focuses exclusively on proto3 syntax. proto2 files are rejected.
:::

## Installation

Download the binary for your platform from the [GitHub Releases](https://github.com/ghostdogpr/proteus/releases) page:

- `proteus-diff-linux-x86_64`
- `proteus-diff-macos-x86_64`
- `proteus-diff-macos-aarch64`

Make it executable and move it to a directory on your `PATH`:

```bash
chmod +x proteus-diff-macos-aarch64
mv proteus-diff-macos-aarch64 /usr/local/bin/proteus-diff
```

Verify the installation:

```bash
proteus-diff --version
```

## Quick start

Compare two proto files:

```bash
proteus-diff old.proto new.proto
```

Compare two directories of proto files (recursive):

```bash
proteus-diff old/ new/
```

The tool prints a grouped report and exits with code `1` if any breaking change is found, `0` otherwise — so it plugs directly into CI.

## Comparing git refs

You can pass git refs directly — branches, tags, or commits — instead of filesystem paths:

```bash
proteus-diff main HEAD              # compare PR against main
proteus-diff HEAD~1 HEAD            # what changed in the last commit
proteus-diff v0.1.0 v0.2.0          # compare two releases
proteus-diff main ./proto           # ref vs current working tree
```

Resolution rule: if the argument exists as a file or directory, it's treated as a filesystem path. Otherwise, proteus-diff tries to resolve it as a git ref with `git rev-parse`. If neither works, you get an error.

::: tip
If a file or directory happens to share a name with a ref (e.g. a folder named `main`), the filesystem wins by default. Use the `git:` prefix to force git mode:

```bash
proteus-diff git:main ./proto     # force git ref
```
:::

### Git submodules

If your protos live in a git submodule, `cd` into the submodule and run proteus-diff with the submodule's own refs:

```bash
cd path/to/submodule
proteus-diff main HEAD
```

Git commands resolve correctly against the submodule's repository.

## Compatibility modes

proteus-diff evaluates each change against one of three compatibility axes. Pick the one that matches what your consumers rely on:

- **wire** — field numbers, wire types, and enum numeric values matter. Renaming a field is fine (binary compatibility is preserved); changing a field number is not.
- **source** — field names, type names, and declaration order matter. Renaming a field is breaking (source code referencing it will fail); reordering fields is a warning.
- **strictest** — reports the worst of the two. This is the default and recommended when your consumers include both binary-only and source-level clients.

Set the mode with `-m`:

```bash
proteus-diff old.proto new.proto -m wire
```

## Output

### Text format (default)

Changes are grouped by file, then by change type. Each entry is prefixed with its severity:

```
Proto changes (3)
  user.proto
    FieldRemoved (1)
      error: User: field 'email' removed
    FieldAdded (1)
      warning: User: field 'phone' added
    CommentChanged (1)
      info: comment changed on 'User'
```

Severity prefixes — `error:`, `warning:`, `info:` — are colored red / yellow / blue when connected to a terminal. Colors are auto-disabled when the output is piped, so the prefix remains the primary signal.

### JSON format

Use `-f json` for machine-consumable output:

```bash
proteus-diff old.proto new.proto -f json
```

```json
[
  {"type": "FieldRemoved", "severity": "error", "path": ["user.proto", "User"], "message": "user.proto.User: field 'email' removed"},
  {"type": "FieldAdded", "severity": "warning", "path": ["user.proto", "User"], "message": "user.proto.User: field 'phone' added"}
]
```

Pipe this into `jq` or any JSON-aware tool to build custom reporters or CI checks.

### Markdown format

Use `-f markdown` to produce GitHub/GitLab-friendly output, ideal for PR comments:

```bash
proteus-diff main HEAD -f markdown
```

```markdown
## Proto changes (3)

- 🔴 1 error
- 🟡 1 warning
- 🔵 1 info

### `user.proto`

- 🔴 **FieldRemoved** — User: field 'email' removed
- 🟡 **FieldAdded** — User: field 'phone' added
- 🔵 **CommentChanged** — comment changed on 'User'
```

## Options

| Flag                        | Description                                                     | Default     |
| --------------------------- | --------------------------------------------------------------- | ----------- |
| `-m, --mode <mode>`         | Compatibility axis: `wire`, `source`, or `strictest`            | `strictest` |
| `-s, --severity <severity>` | Minimum severity to display: `error`, `warning`, or `info`      | `warning`   |
| `-f, --format <format>`     | Output format: `text`, `json`, or `markdown`                    | `text`      |
| `--fail-on <severity>`      | Exit code 1 if any change at this severity or above is reported | `error`     |
| `-o, --override <entry>`    | Severity override (repeatable) — see below                      | —           |
| `--color <mode>`            | Color output: `auto`, `always`, or `never`                      | `auto`      |
| `-v, --version`             | Print version and exit                                          | —           |
| `-h, --help`                | Print help and exit                                             | —           |

## Exit codes

- `0` — no changes at or above the `--fail-on` severity
- `1` — at least one change at or above the `--fail-on` severity
- `2` — invalid arguments or file/parse errors

## Severity overrides

The built-in severity mapping reflects common expectations (removing a field is an error, adding one is a warning, etc.), but your team may disagree on some cases. Use `-o` to override individual change types per mode:

```bash
proteus-diff old.proto new.proto \
  -o wire.FieldRemoved=info \
  -o source.FieldRenamed=warning
```

The format is `mode.ChangeType=severity`, where:
- `mode` is `wire` or `source` (`strictest` is derived from the other two)
- `ChangeType` is one of the values below
- `severity` is `error`, `warning`, or `info`

The flag is repeatable. Unspecified change types keep their defaults.

### Change types

| Category           | Types                                                                                                                                                                              |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| File / package     | `FileAdded`, `FileRemoved`, `PackageChanged`                                                                                                                                       |
| Import             | `ImportAdded`, `ImportRemoved`, `ImportModifierChanged`                                                                                                                            |
| Message            | `MessageAdded`, `MessageRemoved`, `MessageRenamed`, `MessageMoved`                                                                                                                 |
| Field              | `FieldAdded`, `FieldRemoved`, `FieldNumberChanged`, `FieldRenamed`, `FieldTypeChanged`, `FieldTypeRefRenamed`, `FieldOptionalityChanged`, `FieldOrderChanged`, `FieldOneOfChanged` |
| Enum               | `EnumAdded`, `EnumRemoved`, `EnumRenamed`, `EnumMoved`, `EnumValueAdded`, `EnumValueRemoved`, `EnumValueNumberChanged`, `EnumValueRenamed`                                         |
| Reserved / options | `ReservedAdded`, `ReservedRemoved`, `OptionAdded`, `OptionRemoved`, `OptionChanged`                                                                                                |
| Service / RPC      | `ServiceAdded`, `ServiceRemoved`, `RpcAdded`, `RpcRemoved`, `RpcRequestTypeChanged`, `RpcResponseTypeChanged`, `RpcStreamingChanged`                                               |
| Comments           | `CommentAdded`, `CommentRemoved`, `CommentChanged`                                                                                                                                 |

## Examples

**CI check — fail on any breaking change:**

```bash
proteus-diff proto/main proto/pr
```

**Fail on warnings too (stricter policy):**

```bash
proteus-diff old/ new/ --fail-on warning
```

**Show everything including info-level changes:**

```bash
proteus-diff old/ new/ -s info
```

**Wire-only compatibility check (good for binary-only consumers):**

```bash
proteus-diff old/ new/ -m wire
```

**Machine-readable output for a custom reporter:**

```bash
proteus-diff old/ new/ -f json | jq '.[] | select(.severity == "error")'
```

**Per-team severity policy — renaming fields is fine in source mode:**

```bash
proteus-diff old/ new/ -m source -o source.FieldRenamed=info
```

## Notes on detection

- **Message / enum renames** are detected by comparing the full structure (fields, values, options). A renamed type with identical contents is reported as `MessageRenamed` / `EnumRenamed` rather than add + remove.
- **Cross-file moves** are detected when comparing directories: a top-level type that moved between files is reported as `MessageMoved` / `EnumMoved`.
- **Type-ref renames** (e.g. a field referring to a renamed message) are reported as `FieldTypeRefRenamed`, not `FieldTypeChanged`, so they don't block wire compatibility.
- **Removing a field and reserving its number** is treated as a safe change on the wire axis: `FieldRemoved` / `EnumValueRemoved` drops from error to info when the same number appears in a `reserved` declaration in the new schema. Removing without reserving stays an error, since the number could later be reused with a different type and break existing clients.
