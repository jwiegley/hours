# hours

I've been tracking my work hours for years, and this is the tool I built to do
it. At its core, it's a budgeting algorithm -- it takes two sets of intervals
(what you're supposed to do vs. what you've actually done) and tells you where
you stand.

The nice thing about the design is that the core algorithm in `Hours.Budget`
and `Hours.Calc` doesn't actually care about time. It just compares two lists
of intervals and computes statistics about progress. I happen to use it for
tracking work hours, but you could budget money, gas, water, or anything else
you can break into intervals.

## What it looks like

The end result is a tiny indicator in my Emacs modeline:

    6.2h | 8.4

That tells me I've got 6.2 hours left to work today to stay on track, and that
at my current pace I'll need 8.4 hours per remaining work day. The `|` means
it's a regular work day (you'll see `?` for holidays, `!` for off-Fridays, and
so on), and the color shifts from green to red depending on whether I'm ahead
or behind.

One quick glance tells me:

  1. How much I should work today before calling it quits.
  2. Whether I'm on track for the whole period.
  3. If I'm digging myself into long nights down the road.

## The three programs

- **process-hours**: The main tool. It takes ideal and real intervals as JSON,
  runs them through the budget algorithm, and spits out Emacs Lisp for the
  modeline display.

- **work-periods**: Generates the ideal work intervals based on a schedule.
  This one's pretty specific to my setup, but you'd copy and modify it for
  your own work calendar.

- **timelog-periods**: Converts Emacs timeclock format (`i DATE TIME` /
  `o DATE TIME`) into interval JSON. I use
  [org2tc](https://github.com/jwiegley/org2tc) to get timeclock data out of
  Org-mode CLOCK entries.

## Data flow

```
Org-mode → org2tc → timeclock → timelog-periods → real.json
                                                       ↓
work-periods → ideal.json → process-hours → Lisp → Emacs modeline
```

## Building

The project uses Nix for reproducible builds:

```bash
nix develop          # enter the development shell
cabal build all      # build everything
cabal test           # run the test suite
```

You can also build directly with Nix:

```bash
nix build                # build the default executable
nix flake check          # run all checks: build, tests, formatting, linting
```

### Pre-commit hooks

The project uses [Lefthook](https://github.com/evilmartians/lefthook) for
pre-commit checks. After entering the dev shell:

```bash
lefthook install
```

This sets up automatic formatting, linting, and test checks on every commit.

### Formatting

All Haskell code is formatted with
[fourmolu](https://github.com/fourmolu/fourmolu). To format everything:

```bash
./scripts/format.sh
```

## Quick start

```bash
# Generate the ideal schedule
work-periods --there > ideal.json

# Convert Org-mode clocking data
org2tc -s "2024-01-01" todo.org | timelog-periods --file - > real.json

# See where you stand
process-hours --ideal ideal.json --real real.json
```

Or just use the `jobhours` wrapper that does it all in one step:

```bash
jobhours todo.org
```

## Emacs integration

The included `jobhours.el` handles calling `jobhours` periodically and
displaying the result in your modeline. See the file for setup details.

## License

BSD 3-Clause. See [LICENSE.md](LICENSE.md).
