# Project Frame Sessions Hardening Plan

## Scope

This plan addresses the following gaps without changing the package's user documentation:

1. Reject unsafe storage layouts, including symlinked managed roots and ancestors.
2. Enforce that the session store is local and suitable for transactional filesystem operations.
3. Validate timer, retry, and lock-related customization values.
4. Add real graphical end-to-end tests.
5. Add separate-process concurrency tests.
6. Add systematic crash-boundary and recovery tests.
7. Add automated compatibility and package-quality checks.

The existing storage schema and public commands should remain compatible unless a security invariant requires rejecting a previously accepted unsafe configuration.

## General constraints

- Preserve Emacs 29.1 compatibility.
- Keep committed snapshots recoverable after every failed or interrupted mutation.
- Never follow symlinks while writing, moving, recovering, or deleting package-owned data.
- Do not silently replace a corrupt index or unsafe filesystem object.
- Unit tests must remain runnable in batch mode without a graphical display.
- Graphical tests must skip explicitly when no graphical display is available.
- Tests must not read or mutate the user's real session directory.
- Byte compilation must produce no warnings.

## Phase 1: Centralize store and path validation

### 1.1 Define filesystem invariants

Add internal validation helpers to `project-frame-sessions.el` for these package-owned paths:

- `project-frame-sessions-directory`
- `sessions/`
- `trash/`
- `recovery/`
- `.transaction-lock/`
- individual session directories
- individual trash and recovery directories
- snapshot and metadata files

Required invariants:

- The configured store is an absolute local path.
- `file-remote-p` is nil for the store and every derived path.
- An existing store root is a real directory, not a symlink.
- No existing package-managed ancestor between the store and a target is a symlink.
- A target is lexically and canonically contained by its expected parent.
- Destructive operations only affect direct children whose names match the expected schema.
- Existing regular files used as snapshots or metadata are not symlinks.

### 1.2 Add reusable helpers

Introduce narrowly scoped helpers, for example:

- `project-frame-sessions--validate-store`
- `project-frame-sessions--assert-local-path`
- `project-frame-sessions--assert-real-directory`
- `project-frame-sessions--assert-safe-managed-root`
- `project-frame-sessions--assert-contained-path`
- `project-frame-sessions--assert-safe-target-ancestry`

Exact names may differ, but checks must not be duplicated inconsistently across save, restore, delete, recovery, purge, and locking code.

Avoid relying only on `expand-file-name` or string-prefix checks. Account for:

- `..` components
- sibling names with a common prefix
- symlinked parents
- nonexistent final targets whose existing ancestors still need validation
- a store path that becomes unsafe after mode activation

### 1.3 Apply validation at operation boundaries

Validate the store before every operation that reads or mutates persistent state:

- index read and write
- lock acquisition
- save staging and commit
- restore
- rename
- soft delete and delete-all
- trash recovery
- interrupted-save recovery
- purge
- recovery scanning and cleanup

Revalidate while holding the transaction lock before mutations. Initial validation outside the lock may improve error messages but must not be the sole safety check.

### 1.4 Preserve safe failure behavior

When validation fails:

- Signal a clear error naming the unsafe path and violated invariant.
- Do not create, rename, overwrite, or delete anything.
- Do not treat the store as empty.
- Do not detach a live frame from its current committed session.
- Keep staged or trash data discoverable when it was created before the failure.

### 1.5 Unit tests

Add ERT coverage for at least:

- remote/TRAMP store rejection
- relative store rejection
- store root symlink rejection
- `sessions/` symlink rejection
- `trash/` symlink rejection
- `recovery/` symlink rejection
- lock-root symlink rejection
- nested symlink ancestor rejection
- ordinary safe directories remaining accepted
- nonexistent safe targets under validated parents
- prefix-confusion paths such as `sessions-other/`
- `..` path escape attempts
- no outside file changes after each rejected operation

## Phase 2: Validate customization values

### 2.1 Define accepted values

Use these invariants unless implementation behavior requires a stricter rule:

- `project-frame-sessions-autosave-interval`: nil or a positive number
- `project-frame-sessions-debounce-delay`: a nonnegative number
- `project-frame-sessions-maximum-dirty-age`: a nonnegative number
- `project-frame-sessions-retry-delays`: a nonempty list of nonnegative numbers, or explicitly support nil as “no automatic retry”
- `project-frame-sessions-ownerless-lock-stale-seconds`: a nonnegative number

Decide and test whether zero means “run immediately.” Do not allow zero for a repeating idle timer if it can create a busy loop.

### 2.2 Enforce values in two layers

- Narrow each `defcustom` type where Emacs customization types can express the invariant.
- Add runtime validation because variables can be set with `setq`, file-local evaluation, tests, or Lisp code that bypasses Customize.

Perform validation when enabling the mode and immediately before values are used by timer or lock logic. Report the variable name and invalid value.

### 2.3 Tests

Cover:

- negative values
- zero interval behavior
- empty retry list behavior
- nonnumeric retry members
- invalid values set before mode activation
- invalid values introduced while the mode is active
- valid integer and floating-point values
- retry-delay exhaustion and reuse of the final delay

## Phase 3: Crash-boundary fault-injection tests

### 3.1 Add deterministic failpoints for tests

Provide an internal test-only mechanism that can raise an error at named transaction boundaries without changing normal behavior. Prefer a dynamically bound internal callback or hook over production conditionals scattered through the code.

Candidate save failpoints:

1. after lock acquisition
2. after stage creation
3. after Desktop snapshot creation
4. after staged snapshot validation
5. after recovery metadata write
6. after snapshot promotion
7. before index write
8. after index write
9. before old snapshot cleanup
10. before stage cleanup

Add equivalent failpoints for:

- soft deletion
- trash recovery
- interrupted-save recovery
- purge where partial progress is possible

### 3.2 Assert transaction invariants

For every failpoint, test that a fresh package state can determine one of these outcomes:

- the previous index and snapshot remain authoritative; or
- the new committed index and snapshot are authoritative; or
- uncommitted data is present in validated recovery storage and can be explicitly recovered.

Also assert:

- the index remains parseable
- no index entry points to a missing snapshot
- no two active entries share a name or ID
- old committed data is not deleted before the new index commits
- transaction locks are released after ordinary signaled errors
- unrelated session data is unchanged

### 3.3 Fresh-process recovery verification

Do not rely only on in-memory state after fault injection. For persistence-critical cases, start a new batch Emacs process against the temporary store and run recovery scanning or invariant checks there.

## Phase 4: Separate-process concurrency tests

### 4.1 Build a subprocess test harness

Add test helpers that launch isolated `emacs -Q --batch` processes with:

- the package directory on `load-path`
- a shared temporary session store
- unique temporary user directories
- deterministic scripts and exit codes
- bounded timeouts
- captured stdout and stderr for failure diagnostics

Do not use the developer's active Emacs server or user configuration.

### 4.2 Test lock exclusion

Add scenarios for:

- process A holds the lock while process B attempts a transaction
- B fails without modifying the index or A's staged data
- B succeeds after A releases the lock
- an owner file is published and parsed correctly
- a fresh ownerless lock is not reclaimed
- a stale ownerless lock is reclaimed
- a dead local owner is reclaimed
- a foreign-host owner is conservatively treated as live
- a symlinked lock is never reclaimed or followed

### 4.3 Test concurrent state changes

Add scenarios where two processes attempt:

- saves to different sessions
- saves to the same session
- first saves using the same proposed name
- rename versus save
- delete versus save
- recovery versus a new save

Expected results must be explicit. At minimum, serialization must prevent lost index updates, duplicate active identities, and snapshots referenced by no recoverable metadata after interruption.

### 4.4 PID reuse limitation

Review whether PID plus hostname is sufficient for stale-owner detection. If practical, include a per-process random token and process start information in owner metadata. If process start time cannot be checked portably, retain conservative behavior and cover it with tests.

## Phase 5: Graphical end-to-end tests

### 5.1 Keep graphical tests separate

Create a separate graphical test file, for example:

- `project-frame-sessions-graphical-tests.el`

Each test should skip with `ert-skip` unless `display-graphic-p` is non-nil. Keep the normal batch unit suite display-independent.

### 5.2 Minimum end-to-end scenarios

Using real frames and real Desktop/Frameset behavior, test:

1. Create a graphical frame, buffers, windows, and multiple tabs.
2. Save the frame as a session without mocking Desktop.
3. Mutate or close the frame.
4. Restore into the current frame and verify buffers, selected tab, and window layout.
5. Restore into a newly created frame and verify frame identity.
6. Confirm a restore failure deletes only frames created by that failed restore.
7. Close an enrolled frame and verify the close hook saves first.
8. Verify two live frames do not acquire the same session identity.
9. Verify omitted tabs are excluded while at least one valid current tab remains.

### 5.3 Optional Tabspaces coverage

When Tabspaces is installed:

- create workspace associations in hidden tabs
- save and restore them
- verify frame-local workspace buffer associations

Skip these tests when Tabspaces is unavailable; do not add it as a required runtime dependency unless that is an intentional package decision.

### 5.4 Restart-level scenario

Add at least one harness test that:

- saves a real graphical session in one Emacs process
- exits
- starts another graphical Emacs process against the same temporary store
- restores explicitly
- verifies the resulting state

This test may be placed in a slower or optional CI job.

## Phase 6: Compatibility and quality automation

### 6.1 Add reproducible commands

Add project automation that runs:

- unit ERT tests
- graphical ERT tests when a display is available
- byte compilation
- Checkdoc
- package-lint when available

Warnings from byte compilation should fail CI.

### 6.2 CI matrix

Run at least:

- the minimum supported Emacs 29.x version available to CI
- the latest Emacs 29.x
- Emacs 30.x

Use a virtual display such as Xvfb for the graphical job. Keep subprocess concurrency tests bounded to avoid hanging CI.

### 6.3 Test isolation checks

CI should verify that tests:

- use temporary stores
- leave no Emacs subprocesses running
- clean temporary frames and directories
- do not load the user's init file
- do not depend on test execution order

Randomize ERT order where practical, or run persistence-sensitive groups independently.

## Phase 7: Final verification

Before considering the work complete, verify all of the following:

- Existing 42 unit tests still pass.
- New path-hardening tests pass.
- New customization validation tests pass.
- Every injected transaction failure satisfies persistence invariants.
- Separate-process lock and concurrency tests pass repeatedly.
- Graphical tests pass under a virtual or real graphical display.
- Byte compilation is warning-free on all supported Emacs versions.
- Checkdoc and package-lint pass.
- No public command writes through a symlinked or remote managed root.
- A corrupt index is never interpreted as an empty index.
- A failed save never destroys the last committed snapshot.
- A failed delete or recovery remains discoverable and recoverable.

## Suggested implementation order

1. Add centralized store/path validators and their unit tests.
2. Route every persistent operation through those validators.
3. Add customization validation and tests.
4. Introduce test failpoints and complete crash-boundary coverage.
5. Add the separate-process harness and lock/concurrency tests.
6. Add graphical end-to-end tests.
7. Add CI and quality gates.
8. Run the full verification matrix and address any compatibility differences.

## Definition of done

The package is complete for this hardening scope when unsafe stores are rejected before mutation, configuration cannot schedule pathological timers, transaction behavior is verified across injected crashes and real process contention, real graphical save/restore behavior is exercised, and the supported Emacs versions pass the automated quality matrix.
