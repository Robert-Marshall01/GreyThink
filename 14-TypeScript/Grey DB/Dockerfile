# ---- Build Stage ----
FROM node:20-alpine AS build
WORKDIR /app
COPY package.json package-lock.json* ./
COPY packages/core/package.json packages/core/
COPY packages/server/package.json packages/server/
COPY packages/ui/package.json packages/ui/
RUN npm install --workspaces --include-workspace-root
COPY tsconfig.base.json ./
COPY packages/ packages/
RUN npm run build --workspace=packages/core
RUN npm run build --workspace=packages/server

# ---- Server ----
FROM node:20-alpine AS server
WORKDIR /app
COPY --from=build /app/packages/core/dist packages/core/dist
COPY --from=build /app/packages/core/package.json packages/core/
COPY --from=build /app/packages/server/dist packages/server/dist
COPY --from=build /app/packages/server/package.json packages/server/
COPY --from=build /app/node_modules node_modules
COPY --from=build /app/package.json .
EXPOSE 4000
CMD ["node", "packages/server/dist/index.js"]

# ---- UI ----
FROM node:20-alpine AS ui
WORKDIR /app
COPY --from=build /app/packages/ui packages/ui
COPY --from=build /app/node_modules node_modules
COPY --from=build /app/package.json .
WORKDIR /app/packages/ui
EXPOSE 3000
CMD ["npx", "vite", "--host", "0.0.0.0", "--port", "3000"]
