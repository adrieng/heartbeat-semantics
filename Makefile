TARGET=heartbeat.native

GRAPHVIZ=dot
DOTS= \
	test.0.none.dot \
	test.1.eager.dot \
	test.2.amo-outermost.dot \
	test.3.amo-innermost.dot \
	test.4.amo-middle.dot \
	test.5.tresh-outermost.dot \
	test.6.tresh-innermost.dot \
	test.7.tresh-middle.dot
PNGS=$(DOTS:.dot=.png)

.PHONY: all realclean images clean view fake
.PREVIOUS: $(DOTS)

all: $(TARGET)

images: $(PNGS)

clean:
	rm -f $(DOTS) $(PNGS)
	rm -rf _build

realclean: clean
	rm -f $(TARGET)

view: $(filter $(TARGET),.png)

%.png: %.dot
	$(GRAPHVIZ) -Tpng $^ > $@

%.dot: $(TARGET)
	./$^

fake:

%.native: fake
	ocamlbuild src/$@
