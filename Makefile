# SPDX-FileCopyrightText: 2026 Elias Khanzada
# SPDX-License-Identifier: GPL-3.0-or-later

# Stones -- build the program and run its tests.
#
# Everything here assumes you are inside `nix develop`, which supplies
# GHC with the gi-gtk 4 bindings, the GTK libraries they load, and GNU
# Go to play against.
#
# GHC is called directly rather than through cabal. The declarative GTK
# layer lives in a checkout beside this one rather than on Hackage, and
# naming its source directories on the command line builds against it
# without making cabal build and register three packages first. Use
# `cabal build all` if you would rather have that; cabal.project names
# the same three directories.

BUILD := .build

SRC     := src
APP     := app
TEST    := test
WIDGET  := widget-test
INPUT   := input-test

# Where the declarative GTK layer is. Point this somewhere else if your
# checkout is not beside this one.
DECLARATIVE ?= ../gi-gtk-declarative
GTKD := $(DECLARATIVE)/gi-gtk4-declarative/src \
        $(DECLARATIVE)/gi-gtk4-declarative-app-simple/src \
        $(DECLARATIVE)/gi-gtk4-declarative-adwaita/src
INCLUDES := $(addprefix -i,$(SRC) $(GTKD))

# What every module here is written against, said once rather than at
# the top of all forty-seven of them. The cabal file says the same, in
# default-extensions, so the two builds agree.
#
# NoFieldSelectors is not among them, and cannot be: these flags reach
# the declarative layer as well, which this file compiles from source,
# and that layer uses its field selectors as functions. The modules
# here that want it say so themselves.
EXTENSIONS := \
  -XDuplicateRecordFields \
  -XOverloadedLabels \
  -XOverloadedLists \
  -XOverloadedRecordDot \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTemplateHaskell

# What the declarative layer's own cabal file turns on for its library,
# which its modules therefore do not mention. Compiling it from source
# rather than as a package means saying them here, and this list has to
# follow that one.
DECLARATIVE_EXTENSIONS := \
  -XAllowAmbiguousTypes \
  -XDefaultSignatures \
  -XFunctionalDependencies \
  -XMultiWayIf \
  -XTypeFamilies \
  -XUndecidableInstances

# GHC2024, because the declarative layer is GHC2024 and this file
# compiles it alongside Stones. Its modules lean on that edition for a
# bare `forall` and for promoted constructors, and say so nowhere,
# because their cabal file says it for them. An older edition here does
# not ask less of Stones, it asks the impossible of the library: this
# build worked only while interface files from an older one were lying
# about, and failed as soon as the build directory was emptied.
#
# A record built without all of its fields is a warning by default and
# an exception when the field is read, which is a long way from where
# the field was left out. A field added to a record is exactly when
# that happens, so it is an error here.
WARNINGS := -Wall -Werror=missing-fields -XGHC2024 \
            $(EXTENSIONS) $(DECLARATIVE_EXTENSIONS)

# Two packages in the dev shell hold a module called GI.Gtk: gi-gtk,
# which is the one the cabal file names, and gi-gtk4, a copy of it under
# another name. Hiding the copy is what makes an import of GI.Gtk
# unambiguous when GHC is called directly.
PACKAGES := -hide-package gi-gtk4 -hide-package gi-gdk4

# GHC optimises nothing unless it is asked.
#
# `cabal build` asks for -O on its own, so a Makefile that did not ask
# would leave only this build unoptimised, which is the half nobody
# would think to check. This one does ask. What it did not do until
# now is depend on the file the flag lives in, so changing the flag
# rebuilt nothing and the old binary stayed where it was.
#
# What it costs here, timed on clean builds of the program:
#
#   -O0    31 s
#   -O     75 s
#   -O2    88 s
#
# Most of that is the declarative layer, which this file compiles from
# source along with Stones, so the three move together whatever Stones
# itself is doing.
#
# -O2 is what this build has always asked for and it is kept. What it
# buys over -O has not been measured here, and it is not obvious: a
# board is 361 points and the rules over them are cheap, so what runs
# often enough to matter is the drawing. `make profile` is how to
# answer that rather than guess at it.
OPTIMISATION := -O2

# -M caps the compiler's heap, so a compile that runs away dies with a
# heap overflow message instead of growing until the kernel kills
# something else on the machine. Every call here loads the whole gi-gtk
# interface, which is large.
GHC_RTS := +RTS -M4g -A64m -RTS

SOURCES := $(shell find $(SRC) -name '*.hs')

COVERAGE := $(BUILD)/coverage

# The modules to measure, which are this program's own. Everything else
# the test binary is built from is the declarative GTK layer, whose
# coverage is its own repository's business.
MEASURED := $(shell find $(SRC) -name '*.hs' \
  | sed -e 's|^$(SRC)/||' -e 's|/|.|g' -e 's|\.hs$$||' -e 's|^|--include=|')

# A nested X server, which is all the widget tests need: they build
# widgets from code and read them back, so nothing has to be on screen,
# but GTK still refuses to start without a display.
XVFB := xvfb-run -s "-screen 0 1280x1024x24"

.PHONY: all build stones ui check check-pure check-widget check-input \
        check-cabal coverage profile profile-heap run clean

# One compiler at a time. Each call below loads the whole gi-gtk
# interface, so `make -j` multiplies the memory rather than dividing the
# time.
.NOTPARALLEL:

all: stones

# The markup, compiled from Blueprint.
#
# The compiled files are checked in, so that a copy built with cabal
# alone runs without blueprint-compiler. Run this after editing a .blp
# and check the answer in beside it.
UI := $(patsubst %.blp,%.ui,$(wildcard data/ui/*.blp))

ui: $(UI)

data/ui/%.ui: data/ui/%.blp
	blueprint-compiler compile --output $@ $<

# Typecheck without producing code, which is the fast gate while
# working.
build:
	@mkdir -p $(BUILD)
	ghc -fno-code $(INCLUDES) -i$(APP) $(WARNINGS) $(PACKAGES) \
	  -outputdir $(BUILD)/check-objects \
	  $(APP)/Main.hs $(GHC_RTS)

stones: $(BUILD)/stones

# Makefile is a prerequisite because the flags live in it. Changing
# OPTIMISATION has to rebuild, and without this make sees a binary
# newer than every source and does nothing.
#
# The test builds below are left out of that on purpose. They are the
# gate, there are three of them, and they are compiled far more often
# than they are slow.
$(BUILD)/stones: $(SOURCES) $(APP)/Main.hs $(UI) Makefile
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -i$(APP) $(WARNINGS) $(PACKAGES) -threaded \
	  $(OPTIMISATION) \
	  -outputdir $(BUILD)/objects -o $@ $(APP)/Main.hs $(GHC_RTS)

check: check-pure check-widget check-input check-cabal

# The rules, the drawing, the protocol, and what each event does. None
# of these need a display: they are functions, a cairo surface in
# memory, and a subprocess.
check-pure: $(BUILD)/tests
	$(BUILD)/tests

$(BUILD)/tests: $(SOURCES) $(wildcard $(TEST)/*.hs)
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -i$(TEST) $(WARNINGS) $(PACKAGES) -threaded \
	  -outputdir $(BUILD)/test-objects -o $@ $(TEST)/Main.hs $(GHC_RTS)

# The parts that are widgets, which GTK has to be running to build.
check-widget: $(BUILD)/widget-tests
	$(XVFB) $(BUILD)/widget-tests

$(BUILD)/widget-tests: $(SOURCES) $(wildcard $(WIDGET)/*.hs)
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -i$(WIDGET) $(WARNINGS) $(PACKAGES) -threaded \
	  -outputdir $(BUILD)/widget-objects -o $@ $(WIDGET)/Main.hs $(GHC_RTS)

# A click on the board, made with real X11 input.
#
# GTK 4 reports a click through a gesture on the widget, and nothing in
# it can make one happen from code, so this is the only way to reach
# the path from a click to a stone.
check-input: $(BUILD)/input-test
	$(XVFB) tests/gui-input.sh $(BUILD)/input-test

$(BUILD)/input-test: $(SOURCES) $(INPUT)/Main.hs
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -i$(INPUT) $(WARNINGS) $(PACKAGES) -threaded \
	  -outputdir $(BUILD)/input-objects -o $@ $(INPUT)/Main.hs $(GHC_RTS)

# That the package says what it needs.
#
# Everything else here builds with ghc against the shell's package
# database, where every package is there whether this one names it or
# not, so a dependency left out of the cabal file is invisible to it.
# Only cabal notices, because only cabal hides what was not asked for.
check-cabal:
	GHCRTS=-M4g cabal build all

# What the tests reach, by GHC's own counting.
#
# This builds the test program a second time, with -fhpc, because a
# program compiled for coverage is a different program. It is not part
# of `make check` for that reason.
coverage:
	@mkdir -p $(COVERAGE)
	# A .tix file from an earlier run belongs to that run's program, and
	# hpc says so rather than adding the two up.
	@rm -f $(COVERAGE)/*.tix
	ghc -fhpc -hpcdir $(COVERAGE)/mix $(INCLUDES) -i$(TEST) \
	  $(WARNINGS) $(PACKAGES) -threaded \
	  -outputdir $(COVERAGE)/objects -o $(COVERAGE)/tests \
	  $(TEST)/Main.hs $(GHC_RTS)
	ghc -fhpc -hpcdir $(COVERAGE)/mix $(INCLUDES) -i$(WIDGET) \
	  $(WARNINGS) $(PACKAGES) -threaded \
	  -outputdir $(COVERAGE)/widget-objects -o $(COVERAGE)/widget-tests \
	  $(WIDGET)/Main.hs $(GHC_RTS)
	ghc -fhpc -hpcdir $(COVERAGE)/mix $(INCLUDES) -i$(INPUT) \
	  $(WARNINGS) $(PACKAGES) -threaded \
	  -outputdir $(COVERAGE)/input-objects -o $(COVERAGE)/input-test \
	  $(INPUT)/Main.hs $(GHC_RTS)
	# Run from here rather than from the coverage directory, so that
	# what the tests see around them is what they see under `make
	# check`. HPCTIXFILE is what says where the counts are written,
	# which is the only reason the directory came into it.
	HPCTIXFILE=$(COVERAGE)/tests.tix $(COVERAGE)/tests \
	  > $(COVERAGE)/run.log 2>&1
	HPCTIXFILE=$(COVERAGE)/widget-tests.tix $(XVFB) $(COVERAGE)/widget-tests \
	  > $(COVERAGE)/widget-run.log 2>&1
	HPCTIXFILE=$(COVERAGE)/input-test.tix \
	  $(XVFB) tests/gui-input.sh $(COVERAGE)/input-test \
	  > $(COVERAGE)/input-run.log 2>&1
	# The two programs are compiled from the same sources into the same
	# mix directory, so hpc adds their runs up into one report. Each has
	# a module called Main and they are not the same module, which is
	# the one thing hpc cannot add up, so those are left out.
	@hpc sum --union --exclude=Main --output=$(COVERAGE)/all.tix \
	  $(COVERAGE)/tests.tix $(COVERAGE)/widget-tests.tix \
	  $(COVERAGE)/input-test.tix
	@echo
	@echo "== Stones, all told"
	@hpc report $(COVERAGE)/all.tix --hpcdir=$(COVERAGE)/mix \
	  --srcdir=. $(MEASURED)
	@echo
	@echo "== Per module, least covered first"
	@hpc report $(COVERAGE)/all.tix --hpcdir=$(COVERAGE)/mix \
	  --srcdir=. --per-module $(MEASURED) \
	  | grep -B1 'expressions used' | grep -v '^--$$' | paste - - \
	  | sed 's/-----//g' | sort -t'>' -k2 -n

# Where the time goes, and what is on the heap, from a real session.
#
# A build of its own, like the coverage one, so that `make stones`
# stays the fast gate. It covers Stones and the declarative layer
# together, because this file compiles the library from source
# alongside Stones, which is what makes the library's own costs
# visible here.
#
# -fprof-late rather than -fprof-auto: the cost centres go in after
# optimisation, so the program that runs is the program that ships.
# -fprof-auto puts them in first and blocks the inlining that would
# otherwise happen, which measures a different program and blames the
# wrong things.
#
# It is optimised for the same reason, and that is the point rather
# than a detail. A profile of an unoptimised build is a true account
# of a program nobody runs.
#
# The profile is written when the program exits, so close the window
# rather than killing it, or there will be nothing to read.
PROFILE_BIN := $(BUILD)/stones-prof

profile: $(PROFILE_BIN)
	./$(PROFILE_BIN) $(ARGS) +RTS -p -s -RTS
	@echo
	@echo "wrote stones-prof.prof -- the twenty costliest entries:"
	@sed -n '/^COST CENTRE/,$$p' stones-prof.prof | head -22

# The same build, saying what is live on the heap rather than where
# the time went. -hc groups what is live by the cost centre that
# allocated it.
profile-heap: $(PROFILE_BIN)
	./$(PROFILE_BIN) $(ARGS) +RTS -hc -p -s -RTS
	@echo
	@echo "wrote stones-prof.hp and stones-prof.prof"

$(PROFILE_BIN): $(SOURCES) $(APP)/Main.hs $(UI) Makefile
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -i$(APP) $(WARNINGS) $(PACKAGES) -threaded \
	  $(OPTIMISATION) -prof -fprof-late -rtsopts \
	  -outputdir $(BUILD)/prof-objects -o $@ $(APP)/Main.hs $(GHC_RTS)

run: $(BUILD)/stones
	$(BUILD)/stones

clean:
	rm -rf $(BUILD)
