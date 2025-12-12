# Makefile for building and installing the apopt package

# Package name and directory
PKG_NAME = apopt
PKG_DIR = $(PKG_NAME)

# R executable
R = R

.PHONY: all install clean check document build test

# Default target: install the package
all: install

# Generate Rcpp exports and install the package
install:
	@echo "Generating Rcpp exports..."
	cd $(PKG_DIR) && $(R) --quiet -e "Rcpp::compileAttributes()"
	@echo "Installing package..."
	$(R) CMD INSTALL $(PKG_DIR)
	@echo "Package $(PKG_NAME) installed successfully!"

# Generate documentation using roxygen2
document:
	@echo "Generating documentation..."
	cd $(PKG_DIR) && $(R) --quiet -e "roxygen2::roxygenize()"

# Build source package tarball
build:
	@echo "Building package tarball..."
	$(R) CMD build $(PKG_DIR)

# Run R CMD check
check: build
	@echo "Checking package..."
	$(R) CMD check $(PKG_NAME)_*.tar.gz

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(PKG_DIR)/src/*.o $(PKG_DIR)/src/*.so $(PKG_DIR)/src/*.dll
	rm -rf $(PKG_DIR).Rcheck
	rm -f $(PKG_NAME)_*.tar.gz
	@echo "Clean complete!"

# Reinstall (clean + install)
reinstall: clean install

# Remove installed package
uninstall:
	@echo "Uninstalling $(PKG_NAME)..."
	$(R) --quiet -e "remove.packages('$(PKG_NAME)')" || echo "Package not installed"

# Help message
help:
	@echo "Available targets:"
	@echo "  make install    - Generate Rcpp exports and install package (default)"
	@echo "  make document   - Generate documentation using roxygen2"
	@echo "  make build      - Build source package tarball"
	@echo "  make check      - Run R CMD check"
	@echo "  make clean      - Remove build artifacts"
	@echo "  make reinstall  - Clean and reinstall package"
	@echo "  make uninstall  - Remove installed package"
	@echo "  make help       - Show this help message"
