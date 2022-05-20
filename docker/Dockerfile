FROM ocaml/opam:ubuntu-20.10-ocaml-4.02
# Step 1: Installing OCaml dependencies
USER opam
RUN echo "Installing OCaml packages..."
RUN opam switch create 4.02.0
RUN opam install --unlock-base oasis camlp4 batteries ocamlbuild ocamlfind
# Create Workspace
USER root
RUN useradd -ms /bin/bash decentmon
RUN passwd -d decentmon
RUN mkdir -p /home/decentmon
USER decentmon
RUN chown decentmon /home/decentmon
WORKDIR /home/decentmon
# Step 3: Retrieving source files...
ADD _oasis .
COPY init.sh .
COPY experiment_1.sh .
COPY experiment_2.sh .
COPY experiment_3.sh .
COPY alphabets .
ADD src.zip .
USER root
RUN chmod +x init.sh
RUN chmod +x experiment_*.sh
USER decentmon
RUN ./init.sh

