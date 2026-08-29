FROM node:22.12.0-alpine AS builder

WORKDIR /app

COPY package*.json /app/

ARG NPM_REGISTRY

RUN if [ -n "$NPM_REGISTRY" ]; then npm config set registry "$NPM_REGISTRY"; fi
RUN npm install || { cat /root/.npm/_logs/*.log; exit 1; }

COPY app.js build.js index.html *-parser.js vsdx-builder.js test.js test-samples/ ./
RUN node build.js

FROM nginx:1.30.3-alpine3.23

WORKDIR /usr/share/nginx/html
COPY --from=builder /app/dist/index.html ./
COPY --from=builder /app/*js ./
RUN  touch .nojekyll

# HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
# CMD curl -f http://localhost:8080/ || exit 1

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
