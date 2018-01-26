# hours

This program offers a budgeting algorithm, along with a use case for budgeting
time.

The budget algorithm, currently located in `Hours.Budget` and `Hours.Calc`, is
time agnostic, and simply takes two lists of intervals -- a set of ideal
periods, and a set of real periods -- and computes various statistics about
where you are now, where you should be, and if you're on track. This code
could be used for budgeting any resource, really, but I use it right now for
tracking time.

Three programs are built along with this library:

  - `bae-periods`: Computes the current ideal work periods relative to the
    current time, as expected by my employer, BAE Systems. I don't expect
    anyone else to use this program, but you might copy and modify the to suit
    your environment.

  - `utimelog-periods`: Converts a file in the Emacs timeclock format, which
    is just simply `i DATE TIME` for clock-in, and `o DATE TIME` for clock-out
    (and which can also be understood by Ledger) into a set of real intervals.

    I use the program [org2tc](https://github.com/jwiegley/org2tc) to convert
    my Org-mode file with clocking information into a timeclock log, in order
    to feed into this `timelog-periods` program.

  - `process-hours`: Given an ideal set of intervals, and a real set of
    intervals -- and specifically intervals whose ranges are marked by time,
    and whose associated values are "time expected/actually worked during that
    period" -- this program computes the statistics and reports them in an
    Emacs Lisp format, so they can be read and reported using the included
    `jobhours.el` file.

    Note that if you want to build a budgeting program around monetary values,
    or gallons of gas, or water rationing, etc., this program would need to
    change, as well as the programs you use to generate the ideal and real
    data (assuming you don't create it by hand, since it's just JSON).

The final result of all this is a very brief indicator in my Emacs window that
looks something like this:

    6.2h | 8.4

This means that I have 6.2 hours left to work today to "stay on track", and
that my current progress will requires me to work 8.4 hours every interval
(usually a work day) for the remaining intervals of this work period. The
upright bar means that I'm currently in a "work day", and then color of the
display indicates whether I'm falling behind (starts becoming red), or getting
ahead (starts becoming green).

This way, with a moment's glance, I know these things:

  1. How much I should work today before kicking off.
  2. Whether I'm on track towards the end of the period.
  3. If I'm starting create long nights for myself in the near future.
  4. How close I am to overall completion (the background of the begins to
     fill as work intervals are finish, until it's fully colored).
