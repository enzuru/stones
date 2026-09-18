#!/usr/bin/env bash
#
# Click on the board with real X11 input, and read back where the
# stones went.
#
# GTK 4 reports a click through a gesture, and nothing can make one of
# those happen from code, so this is the only way to test the path from
# a click to a stone. Run it under a display; the Makefile runs it
# under Xvfb.

set -u

binary=${1:?usage: gui-input.sh <input-test-binary>}

# A nested X server has no GL worth speaking of, and without these the
# window never appears at all.
export GDK_BACKEND=x11
export GSK_RENDERER=cairo
export GTK_A11Y=none

log=$(mktemp)
# The window is told to close on its standard input rather than killed,
# so that it ends the way a window ends and writes down whatever it was
# asked to write down on the way out.
fifo=$(mktemp -u)
mkfifo "$fifo"
trap 'rm -f "$log" "$fifo"' EXIT

"$binary" < "$fifo" > "$log" 2>&1 &
app=$!
exec 3>"$fifo"

fail () {
  echo "FAIL: $1"
  echo "--- what the window printed:"
  cat "$log"
  exec 3>&- 2>/dev/null
  kill $app 2>/dev/null
  wait $app 2>/dev/null
  exit 1
}

# Wait for the window, for up to ten seconds.
window=
for _ in $(seq 1 20); do
  window=$(xdotool search --name '^Stones$' 2>/dev/null | head -1)
  [ -n "$window" ] && break
  sleep 0.5
done
[ -n "$window" ] || fail "the window never appeared"

xdotool windowfocus --sync "$window" 2>/dev/null
sleep 1

# The player's stones, as the window last reported them.
stones () {
  grep '^BOARD ' "$log" | tail -1 | cut -d' ' -f2-
}

# How many of them there are.
howMany () {
  set -- $(stones)
  if [ "$*" = "-" ]; then echo 0; else echo $#; fi
}

click () {
  xdotool mousemove --window "$window" "$1" "$2" click 1
  sleep 1.5
}

[ "$(howMany)" -eq 0 ] \
  || fail "there were stones on the board before any click: $(stones)"

click 200 300
first=$(stones)
[ "$(howMany)" -eq 1 ] || fail "a click on the board played nothing"

click 400 550
second=$(stones)
[ "$(howMany)" -eq 2 ] \
  || fail "a click somewhere else did not play a second stone: $second"
[ "$second" != "$first" ] \
  || fail "two clicks in different places played the same point ($first)"

# The same point again. It has a stone on it now, so nothing is played.
click 400 550
again=$(stones)
[ "$again" = "$second" ] \
  || fail "clicking a point that has a stone played something ($again)"

echo quit >&3
exec 3>&-
wait $app 2>/dev/null
echo "gui-input: passed, played $second"
