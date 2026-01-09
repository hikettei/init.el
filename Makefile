install:
	@mkdir -p ~/.emacs.d/hikettei
	@mkdir -p ~/.emacs.d/agents
	@cp ./init.el ~/.emacs.d/init.el
	@cp ./pyproject.toml ~/.emacs.d/pyproject.toml
	@cp ./agents/*.json ~/.emacs.d/agents/
	@cp -r ./hikettei/ ~/.emacs.d/hikettei/
