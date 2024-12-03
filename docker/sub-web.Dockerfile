FROM node:14-alpine as builder

WORKDIR /app
COPY . .
RUN yarn install
RUN yarn build

FROM nginx:alpine
COPY --from=builder /app/dist /usr/share/nginx/html
COPY docker/nginx.conf /etc/nginx/conf.d/default.conf 