.PHONY: help lint lint test doc doc-clean doc-build doc-view pre-push-checks clean

help:
	@echo "Available commands:"
	@echo "  make lint            - Run all linters and auto-fix issues"
	@echo "  make test            - Run pytest"
	@echo "  make doc             - Build and view documentation"
	@echo "  make doc-clean       - Clean documentation build artifacts"
	@echo "  make doc-build       - Build documentation"
	@echo "  make doc-view        - View documentation in browser"
	@echo "  make pre-push-checks - Run all checks before pushing"
	@echo "  make clean           - Clean build artifacts"

# Linting
lint:
	@echo "Running linters..."
	uv run --group dev pre-commit run --all-files

# Testing
test:
	# uv run --group test pytest -n auto --maxprocesses 16
	uv run --group test pytest tests/unit/test_scheduler.py::TestScheduler::test_successfully_select_host_from_list

# Documentation
doc-clean:
	rm -rf docs/_build/ docs/tactus.rst docs/markdown_docs/config.md

doc-build: doc-clean
	uv run --group doc tactus doc config >| docs/markdown_docs/config.md
	uv run --group doc python docs/write_output_overview.py docs/markdown_docs/output_overview.md
	uv run --group doc sphinx-apidoc tactus -o docs/ --force --no-toc --module-first
	uv run --group doc sphinx-build docs docs/_build/
	touch docs/_build/.nojekyll

doc-view:
	@if [ ! -f docs/_build/index.html ]; then \
		$(MAKE) doc-build; \
	fi
	@uv run python -c "import webbrowser; webbrowser.open('docs/_build/index.html')"

doc: doc-build doc-view

# Pre-push checks
pre-push-checks: lint doc-clean doc-build test
	@echo "All pre-push checks passed!"

# Clean
clean:
	rm -rf docs/_build/ docs/tactus.rst docs/markdown_docs/config.md docs/markdown_docs/output_overview.md
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete
	rm -rf .pytest_cache .coverage .coverage.xml
