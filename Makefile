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

SRC  := src
APP  := app
TEST := test

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

.PHONY: all build stones check run clean

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

# The rules, the protocol, and the geometry. None of these need a
# display: they are pure functions and a subprocess.
check: $(BUILD)/tests
	$(BUILD)/tests

$(BUILD)/tests: $(SOURCES) $(wildcard $(TEST)/*.hs)
	@mkdir -p $(BUILD)
	ghc $(INCLUDES) -i$(TEST) $(WARNINGS) $(PACKAGES) -threaded \
	  -outputdir $(BUILD)/test-objects -o $@ $(TEST)/Main.hs $(GHC_RTS)

run: $(BUILD)/stones
	$(BUILD)/stones

clean:
	rm -rf $(BUILD)
