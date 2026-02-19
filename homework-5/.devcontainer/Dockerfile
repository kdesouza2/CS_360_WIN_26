FROM racket/racket:8.15-full
RUN apt-get update && apt-get install -y \
  git \
  libedit2 \
  make \
  && rm -rf /var/lib/apt/lists/*
RUN useradd -ms /bin/bash drexelpl
