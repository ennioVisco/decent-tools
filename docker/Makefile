docker: 
	cp ../_oasis .
	zip src.zip ../src/*.ml
	docker build -t decentmon:1 .

run:
	docker run -ti decentmon:1

.PHONY: clean

clean:
	rm -f _oasis
	rm -f _tags
	rm -f src.zip
