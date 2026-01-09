EMACS_DIR = ~/.emacs.d
EAF_DIR = $(EMACS_DIR)/site-lisp/eaf

.PHONY: install install-config install-deps install-python-deps install-eaf clean help

# Main install target
install: install-config install-deps
	@echo "Installation complete!"

# Install all including EAF browser
install-all: install-config install-deps install-eaf
	@echo "All installations complete!"

# Copy config files to ~/.emacs.d
install-config:
	@mkdir -p $(EMACS_DIR)/hikettei
	@mkdir -p $(EMACS_DIR)/agents
	@mkdir -p $(EMACS_DIR)/site-lisp
	@cp ./init.el $(EMACS_DIR)/init.el
	@cp ./pyproject.toml $(EMACS_DIR)/pyproject.toml
	@cp ./agents/*.json $(EMACS_DIR)/agents/
	@cp -r ./hikettei/ $(EMACS_DIR)/hikettei/
	@echo "Config files installed to $(EMACS_DIR)"

# Install MCP server dependencies with uv
install-deps:
	@echo "Installing Python dependencies with uv..."
	@cd $(EMACS_DIR) && uv sync
	@echo "Python dependencies installed"

# Install Python dependencies for EAF (system-wide)
install-python-deps:
	@echo "Installing Python dependencies for EAF..."
	pip3 install --user --break-system-packages PyQt6 PyQt6-WebEngine epc sexpdata pycookiecheat
	@echo "Python dependencies installed"

# Install EAF and browser app
install-eaf: install-python-deps
	@echo "Installing EAF (Emacs Application Framework)..."
	@if [ -d "$(EAF_DIR)/.git" ]; then \
		echo "EAF already cloned, updating..."; \
		cd $(EAF_DIR) && git pull; \
	else \
		rm -rf $(EAF_DIR); \
		git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git $(EAF_DIR); \
	fi
	@cd $(EAF_DIR) && python3 ./install-eaf.py -i browser
	@echo "EAF installed"

# Remove EAF
clean:
	@rm -rf $(EAF_DIR)
	@echo "EAF removed"

# Show help
help:
	@echo "Available targets:"
	@echo "  install             - Install config and MCP dependencies (default)"
	@echo "  install-all         - Install everything including EAF browser"
	@echo "  install-config      - Copy config files to ~/.emacs.d"
	@echo "  install-deps        - Install MCP server dependencies (uv)"
	@echo "  install-python-deps - Install Python packages for EAF (PyQt6)"
	@echo "  install-eaf         - Install EAF and browser app"
	@echo "  clean               - Remove EAF installation"
	@echo "  help                - Show this help"
