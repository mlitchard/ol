FROM fpco/stack-build
MAINTAINER Michael Litchard
RUN apt-get upgrade && apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -yq postgresql-client-9.3 git
RUN git clone https://github.com/mlitchard/ol.git
WORKDIR ol
RUN stack install
CMD ["/root/.local/bin/ol"]
