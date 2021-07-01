FROM erlang:24.0.3
RUN apt-get update \
 && apt-get install -y libtinfo5 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN wget -q -O /tmp/hamler.deb https://github.com/hamler-lang/hamler/releases/download/0.4/hamler_0.4_amd64.deb \
 && dpkg -i /tmp/hamler.deb \
 && rm /tmp/hamler.deb
ENTRYPOINT ["hamler"]
