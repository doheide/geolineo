#FROM rust-base-nightly as builder
FROM rust:1.65-slim-buster as builder
WORKDIR app
COPY . .

#RUN true \
#    && apt-get update \
#    && apt-get install -y libpq-dev \
#    && rm -rf /var/lib/apt/lists/*

RUN cargo build --release --bin geolineo --package geolineo


FROM debian:buster-slim as runtime
WORKDIR /etc/geolineo
ENV ROCKET_ENV=production
EXPOSE 80
COPY --from=builder /app/target/release/geolineo /usr/local/bin
COPY --from=builder /app/templates /etc/geolineo/templates
COPY --from=builder /app/geoleo/tests/test_cases /etc/geolineo/test_cases

#RUN true \
#    && apt-get update \
#    && apt-get install -y libpq5 libpq-dev \
#    && rm -rf /var/lib/apt/lists/*
    
ENTRYPOINT ["/usr/local/bin/geolineo"]
