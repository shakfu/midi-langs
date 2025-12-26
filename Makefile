# Makefile frontend for CMake build

BUILD_DIR := build
CMAKE := cmake
CTEST := ctest

.PHONY: all build configure clean test test-quick test-verbose rebuild help

all: build

configure:
	@$(CMAKE) -B $(BUILD_DIR)

build: configure
	@$(CMAKE) --build $(BUILD_DIR) -j4

clean:
	@rm -rf $(BUILD_DIR)

test: build
	@$(CTEST) --test-dir $(BUILD_DIR) --output-on-failure

test-quick: build
	@$(CTEST) --test-dir $(BUILD_DIR) -L quick --output-on-failure

test-verbose: build
	@$(CTEST) --test-dir $(BUILD_DIR) -V

rebuild: clean build

help:
	@echo "Targets:"
	@echo "  all          Build everything (default)"
	@echo "  build        Build all targets"
	@echo "  configure    Run CMake configuration"
	@echo "  clean        Remove build directory"
	@echo "  test         Run all tests"
	@echo "  test-quick   Run quick tests only"
	@echo "  test-verbose Run tests with verbose output"
	@echo "  rebuild      Clean and build"
	@echo ""
	@echo "Executables (after build):"
	@echo "  $(BUILD_DIR)/forth"
	@echo "  $(BUILD_DIR)/midi_forth"
