.PHONY: install install-skill

SKILLS_DIR ?= $(HOME)/.claude/skills

install:
	docker compose build

install-skill:
	@read -p "Skills directory [$(SKILLS_DIR)]: " input; \
	dir=$${input:-$(SKILLS_DIR)}; \
	mkdir -p "$$dir/convert-documents-to-markdown"; \
	cp skills/convert-documents-to-markdown/SKILL.md "$$dir/convert-documents-to-markdown/SKILL.md"; \
	echo "Skill installed to $$dir/convert-documents-to-markdown/"
