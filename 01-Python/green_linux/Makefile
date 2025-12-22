# Makefile - helper targets for development

.PHONY: help extract-pot init-po update-po compile-mo list-langs

help:
	@echo "Makefile targets:"
	@echo "  extract-pot     - Generate locale/powerapp.pot from source"
	@echo "  init-po lang=<xx> - Create a new .po for language code (e.g., lang=es)"
	@echo "  update-po       - Merge locale/powerapp.pot into existing .po files"
	@echo "  compile-mo      - Compile all existing .po into .mo files"
	@echo "  list-langs      - List languages with .po files"

extract-pot:
	python3 scripts/generate_pot.py --output locale/powerapp.pot

init-po:
	@if [ -z "$(lang)" ]; then echo "Specify lang, e.g. make init-po lang=es"; exit 1; fi
	@mkdir -p locale/$(lang)/LC_MESSAGES
	@if command -v msginit >/dev/null 2>&1; then \
		msginit --no-translator --locale=$(lang) --input=locale/powerapp.pot --output-file=locale/$(lang)/LC_MESSAGES/powerapp.po || echo "msginit failed"; \
	else \
		cp locale/powerapp.pot locale/$(lang)/LC_MESSAGES/powerapp.po && echo "msginit not found: copied POT to create initial PO at locale/$(lang)/LC_MESSAGES/powerapp.po"; \
	fi

update-po:
	@if [ ! -f locale/powerapp.pot ]; then echo "Run make extract-pot first"; exit 1; fi
	@for po in $(shell find locale -name 'powerapp.po' -printf '%h/powerapp.po\n'); do \
		langdir=$$(dirname $$po); lang=$$(basename $$(dirname $$langdir)); \
		if command -v msgmerge >/dev/null 2>&1; then \
			msgmerge --update $$po locale/powerapp.pot || echo "msgmerge failed for $$po"; \
		else \
			echo "msgmerge not found; skip updating $$po"; \
		fi; \
	done

compile-mo:
	@for po in $(shell find locale -name 'powerapp.po'); do \
		langdir=$$(dirname $$po); lang=$$(basename $$(dirname $$langdir)); \
		if command -v msgfmt >/dev/null 2>&1; then \
			msgfmt $$po -o $$langdir/powerapp.mo || echo "msgfmt failed for $$po"; \
		else \
			echo "msgfmt not found; cannot compile $$po"; \
		fi; \
	done

list-langs:
	@for d in locale/*; do if [ -d "$$d/LC_MESSAGES" ]; then echo $$(basename $$d); fi; done
