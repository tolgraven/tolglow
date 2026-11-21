# Build explicitly: docker build -t tolglow-dev:local -f dev/container/Dockerfile dev/container
FROM eclipse-temurin:25-jdk-jammy@sha256:43d6f1775a0660b561c1c42edfdf8538a89672a040961acb3730c3ccc53c53de AS java
FROM node:26-bookworm-slim@sha256:5df79989ed685e09f0a98019db4cebfb22fb9df1a53c0161664a011f7ff6b0f1
ARG LEIN_VERSION=2.13.1
ARG LEIN_SHA256=2e4c36cbaef717d0fa589a1f95e4c526a1cb6c35b48dc92e6dbb30d6f8595b5c
ARG CODEX_VERSION=0.171.0
ENV JAVA_HOME=/opt/java/openjdk
ENV PATH=/opt/java/openjdk/bin:$PATH
COPY --from=java /opt/java/openjdk /opt/java/openjdk
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl git zsh zsh-autosuggestions zsh-syntax-highlighting \
      python3 less procps unzip rlwrap && rm -rf /var/lib/apt/lists/*
# Optional corporate build CA is a transient BuildKit secret, never in the image.
RUN --mount=type=secret,id=build_ca \
    if [ -r /run/secrets/build_ca ]; then export CURL_CA_BUNDLE=/run/secrets/build_ca; fi; \
    mkdir -p /usr/local/lib/leiningen \
    && curl -fsSL "https://github.com/technomancy/leiningen/releases/download/${LEIN_VERSION}/leiningen-${LEIN_VERSION}-standalone.jar" -o /usr/local/lib/leiningen/lein.jar \
    && echo "${LEIN_SHA256}  /usr/local/lib/leiningen/lein.jar" | sha256sum --check - \
    && printf '%s\n' '#!/bin/sh' 'exec java -cp /usr/local/lib/leiningen/lein.jar clojure.main -m leiningen.core.main "$@"' > /usr/local/bin/lein \
    && chmod +x /usr/local/bin/lein
RUN --mount=type=secret,id=build_ca \
    if [ -r /run/secrets/build_ca ]; then export NODE_EXTRA_CA_CERTS=/run/secrets/build_ca; fi; \
    npm install --global --no-audit --no-fund "@openai/codex@${CODEX_VERSION}" \
    && npm cache clean --force
COPY zshrc /etc/zsh/zshrc
COPY --chmod=755 entrypoint /usr/local/bin/tolglow-dev-entrypoint
ENV HOME=/home/tolglow TERM=xterm-256color COLORTERM=truecolor GIT_OPTIONAL_LOCKS=0
WORKDIR /tmp
ENTRYPOINT ["/usr/local/bin/tolglow-dev-entrypoint"]
CMD ["/usr/bin/zsh", "-i"]
