FROM ubuntu:24.04
RUN apt-get update && apt-get install -y gfortran make ca-certificates && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY . /src
RUN make build
CMD ["/src/fortran_calculus.exe"]
