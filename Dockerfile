FROM haskell:9.2.5 AS builder

WORKDIR /app
COPY . .
RUN make install

FROM haskell:9.2.5 AS runner
WORKDIR /app

COPY --from=builder /app/bin/2048 ./2048
CMD ["./2048"]
