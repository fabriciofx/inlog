# The MIT License (MIT)
#
# Copyright (C) 2025 Fabrício Barros Cabral
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

RM = rm -f
BUILD = pdflatex
BIB = bibtex
LINT = lacheck
LATEXINDENT = latexindent
BUILD_OPTS := -file-line-error -halt-on-error -interaction=nonstopmode -recorder

.PHONY: all build lint clean show

all: build

build: inlog.tex
	$(BUILD) $(BUILD_OPTS) inlog.tex

lint:
	$(LINT) inlog.tex

format:
	$(LATEXINDENT) -l -m -s -w *.tex

clean:
	$(RM) *.iml .DS_Store *.aux *.log *.bcf *.bbl *.fdb_latexmk *.fls \
		*.run.xml *.blg *.synctex.gz *.bak* *.nav *.toc *.snm *.out \
        inlog.pdf
