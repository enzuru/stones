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

# The cabal file says Haskell2010, so the direct GHC calls say it too,
# rather than building against the newer default and finding out later.
WARNINGS := -Wall -XHaskell2010

# Two packages in the dev shell hold a module called GI.Gtk: gi-gtk,
# which is the one the cabal file names, and gi-gtk4, a copy of it under
# another name. Hiding the copy is what makes an import of GI.Gtk
# unambiguous when GHC is called directly.
PACKAGES := -hide-package gi-gtk4 -hide-package gi-gdk4

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

.PHONY: all build stones check check-pure check-widget check-input coverage run clean

# One compiler at a time. Each call below loads the whole gi-gtk
# interface, so `make -j` multiplies the memory rather than dividing the
# time.
.NOTPARALLEL:

all: stones

# Typecheck without producing code, which is the fast gate while
# working.
build:
	@mkdir -p $(BUILD)
	ghc -fno-code $(INCLUDES) -i$(APP) $(WARNINGS) $(PACKAGES) \
	  -outputdir $(BUILD)/check-objects \
	  $(APP)/Main.hs $(GHC_RTS)

stones: $(BUILD)/stones

$(BUILD)/stones: $(SOURCES) $(APP)/Main.hs
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -i$(APP) $(WARNINGS) $(PACKAGES) -threaded -O2 \
	  -outputdir $(BUILD)/objects -o $@ $(APP)/Main.hs $(GHC_RTS)

check: check-pure check-widget check-input

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
	cd $(COVERAGE) && ./tests > run.log 2>&1
	cd $(COVERAGE) && $(XVFB) ./widget-tests > widget-run.log 2>&1
	cd $(COVERAGE) && $(XVFB) ../../tests/gui-input.sh ./input-test \
	  > input-run.log 2>&1
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

run: $(BUILD)/stones
	$(BUILD)/stones

clean:
	rm -rf $(BUILD)
