FROM ocaml/opam:ubuntu-20.10-ocaml-4.02
# Step 1: Installing OCaml dependencies
USER opam
RUN echo "Installing OCaml packages..."
RUN opam switch create 4.02.0
RUN opam install --unlock-base oasis camlp4 batteries ocamlbuild ocamlfind
# Create Workspace
USER root
RUN useradd -ms /bin/bash decent
RUN passwd -d decent
RUN mkdir -p /home/decent
USER decent
RUN chown decent /home/decent
WORKDIR /home/decent
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
USER decent
RUN ./init.sh

