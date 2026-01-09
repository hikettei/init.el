install:
	@mkdir -p ~/.emacs.d/hikettei
	@cp ./init.el ~/.emacs.d/init.el
	@cp ./pyproject.toml ~/.emacs.d/pyproject.toml
	@cp -r ./hikettei/ ~/.emacs.d/hikettei/
