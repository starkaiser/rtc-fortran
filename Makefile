# ==========================================================
# Ray Tracer Challenge in Fortran
# Options: all (programs + tests), programs only, test, clean
# ==========================================================

FC      = gfortran
FFLAGS  = -O2 -Wall -Wextra -std=f2008 -J$(BUILD_DIR) -I$(BUILD_DIR)

SRC_DIR   = src
TEST_DIR  = tests
PROG_DIR  = programs
BUILD_DIR = build

# -------------------------------
# Files
# -------------------------------
SRC_FILES = \
	$(SRC_DIR)/rtc_utils.f90 \
	$(SRC_DIR)/rtc_tuple.f90 \
	$(SRC_DIR)/rtc_color.f90 \
	$(SRC_DIR)/rtc_canvas.f90 \
	$(SRC_DIR)/rtc_matrix.f90 \
	$(SRC_DIR)/rtc_trans.f90 \
	$(SRC_DIR)/rtc_sphere.f90 \
	$(SRC_DIR)/rtc_ray.f90

TEST_MODULES = \
	$(TEST_DIR)/test_tuple.f90 \
	$(TEST_DIR)/test_color.f90 \
	$(TEST_DIR)/test_canvas.f90 \
	$(TEST_DIR)/test_matrix.f90 \
	$(TEST_DIR)/test_trans.f90 \
	$(TEST_DIR)/test_ray.f90 \
	$(TEST_DIR)/test_sphere.f90

TEST_PROGRAM = $(TEST_DIR)/test_program.f90

PROG_FILES = \
	$(PROG_DIR)/rtc_chapter1.f90 \
	$(PROG_DIR)/rtc_chapter2.f90 \
	$(PROG_DIR)/rtc_chapter3.f90 \
	$(PROG_DIR)/rtc_chapter4.f90 \
	$(PROG_DIR)/rtc_chapter5.f90

# -------------------------------
# Derived build targets
# -------------------------------
SRC_OBJS  = $(patsubst $(SRC_DIR)/%.f90,$(BUILD_DIR)/%.o,$(SRC_FILES))
TEST_OBJS = $(patsubst $(TEST_DIR)/%.f90,$(BUILD_DIR)/%.o,$(TEST_MODULES))
TEST_BIN  = $(BUILD_DIR)/test_program
PROG_BINS = $(patsubst $(PROG_DIR)/%.f90,$(BUILD_DIR)/%,$(PROG_FILES))

# ==========================================================
# Default target: build everything
# ==========================================================
all: programs test

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# -------------------------------
# Compile source modules
# -------------------------------
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# -------------------------------
# Compile test modules
# -------------------------------
$(BUILD_DIR)/%.o: $(TEST_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# -------------------------------
# Build programs only
# -------------------------------
programs: $(BUILD_DIR) $(PROG_BINS)

$(BUILD_DIR)/%: $(PROG_DIR)/%.f90 $(SRC_OBJS)
	$(FC) $(FFLAGS) $(SRC_OBJS) $< -o $@

# -------------------------------
# Build test program
# -------------------------------
$(TEST_BIN): $(TEST_PROGRAM) $(SRC_OBJS) $(TEST_OBJS)
	$(FC) $(FFLAGS) $(SRC_OBJS) $(TEST_OBJS) $< -o $@

# -------------------------------
# Test target: build and run test program
# -------------------------------
.PHONY: test
test: $(TEST_BIN)
	@echo "Running test program..."
	@$(TEST_BIN)

# ==========================================================
# Utility targets
# ==========================================================
.PHONY: clean rebuild

clean:
	rm -rf $(BUILD_DIR)
	@echo "Build directory cleaned."

rebuild: clean all
