PROJECTS = exp_1 exp_2 exp_3 exp_4

PROJECT_BIN_SUFFIX = .bin
PROJECTS_BIN = $(foreach project,$(PROJECTS),$(project)$(PROJECT_BIN_SUFFIX))
PROJECT_PDF_FILES = $(foreach project,$(PROJECTS),$(if $(wildcard $(project)/report.typ),$(project)/report.pdf))
PROJECT_CLEAN_SUFFIX = .clean
PROJECTS_CLEAN = $(foreach project,$(PROJECTS),$(project)$(PROJECT_CLEAN_SUFFIX))

.PHONY: all
all: $(PROJECTS_BIN) $(PROJECT_PDF_FILES)

.PHONY: $(PROJECTS_BIN)
$(PROJECTS_BIN):
	$(MAKE) -C $(@:$(PROJECT_BIN_SUFFIX)=) all

%/report.pdf: %/report.typ
	typst compile --root . --diagnostic-format short --ppi 288 --pdf-standard a-3u $< $@

.PHONY: $(PROJECTS_CLEAN)
$(PROJECTS_CLEAN):
	$(MAKE) -C $(@:$(PROJECT_CLEAN_SUFFIX)=) clean

.PHONY: clean
clean: $(PROJECTS_CLEAN)
	rm -f $(PROJECT_PDF_FILES)
