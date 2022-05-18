# Fourmoulu, cabalfmt, hlint are made available by the nix shell defined in shell.nix
# In most cases you should execute Make after entering nix-shell.

usage:
	@echo "usage: make <command>"
	@echo
	@echo "Available options:"
	@echo "  FLAGS   -- Additional options passed to --ghc-options"
	@echo
	@echo "Available commands:"
	@echo "  build               -- Run cabal v2-build"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  cabalfmt            -- Apply cabal formatting with cabal-fmt"
	@echo "  cabalfmt_check      -- Check cabal files for formatting errors without making changes"
	@echo "  nixpkgsfmt          -- Apply nix formatting with nixfmt"
	@echo "  nixpkgsfmt_check    -- Check nix files for format errors"
	@echo "  format_all          -- Format haskell files, nix files and cabal files"
	@echo "  format_check_all    -- Check haskell files, nix files and cabal files"
	@echo "  lint                -- Check the sources with hlint"
	@echo "  refactor            -- Automatically apply hlint refactors, with prompt"

## Project

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif


project: $(PROJECT_SOURCES) $(CABAL_SOURCES)
PROJECT_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs' )
CABAL_SOURCES := $(shell fd -ecabal)

build: project
	cabal v2-build all $(GHC_FLAGS)

## Formatting

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTypeApplications -o -XImportQualifiedPost
# Extentions we need to tell Hlint about
HLINT_EXTS := # None so far

format_check_all: format_check nixpkgsfmt_check cabalfmt_check
format_all: format cabalfmt nixpkgsfmt

# Run fourmolu formatter
format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(PROJECT_SOURCES)

# Check formatting (without making changes)
format_check: requires_nix_shell
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(PROJECT_SOURCES)

ci: nix build -L .#check.x86_64-linux --experimental-features "nix-command flakes"

# Run cabalfmt formatter
cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

# Check cabalfmt (without making changes)
cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

# Nix files to format
NIX_SOURCES := $(shell fd -enix)

nixpkgsfmt: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check: requires_nix_shell
	nixpkgs-fmt --check $(NIX_SOURCES)

# Check with hlint
lint: requires_nix_shell
	hlint $(HLINT_EXTS) $(PROJECT_SOURCES)

# Apply automatic hlint refactors, with prompt
refactor: requires_nix_shell
	for src in $(PROJECT_SOURCES) ; do \
		hlint $(HLINT_EXTS) --refactor --refactor-options='-i -s' $$src ;\
	done

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ -v IN_NIX_SHELL ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ -v IN_NIX_SHELL ] || (echo "    run 'nix develop .' first" && false)