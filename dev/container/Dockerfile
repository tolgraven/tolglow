# Build explicitly: docker build -t tolglow-dev:local -f dev/container/Dockerfile dev/container
FROM eclipse-temurin:25-jdk-jammy@sha256:43d6f1775a0660b561c1c42edfdf8538a89672a040961acb3730c3ccc53c53de AS java
FROM node:26-bookworm-slim@sha256:5df79989ed685e09f0a98019db4cebfb22fb9df1a53c0161664a011f7ff6b0f1
ARG CLOJURE_VERSION=1.12.7.1681
ARG CLOJURE_SHA256=791c12b5f5a0986ae7456cc50f8db7715992ecc330273305846034ed1bacd6da
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
    curl -fsSL "https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/clojure-tools-${CLOJURE_VERSION}.tar.gz" -o /tmp/clojure.tar.gz \
    && echo "${CLOJURE_SHA256}  /tmp/clojure.tar.gz" | sha256sum --check - \
    && mkdir /tmp/clojure && tar xzf /tmp/clojure.tar.gz -C /tmp/clojure \
    && mkdir -p /usr/local/lib/clojure/libexec /usr/local/share/man/man1 \
    && cp /tmp/clojure/clojure-tools/deps.edn /tmp/clojure/clojure-tools/example-deps.edn /tmp/clojure/clojure-tools/tools.edn /usr/local/lib/clojure/ \
    && cp /tmp/clojure/clojure-tools/*.jar /usr/local/lib/clojure/libexec/ \
    && sed 's@PREFIX@/usr/local/lib/clojure@g' /tmp/clojure/clojure-tools/clojure > /usr/local/bin/clojure \
    && sed 's@BINDIR@/usr/local/bin@g' /tmp/clojure/clojure-tools/clj > /usr/local/bin/clj \
    && chmod +x /usr/local/bin/clojure /usr/local/bin/clj \
    && rm -rf /tmp/clojure /tmp/clojure.tar.gz
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
