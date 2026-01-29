/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Grey API URL - points to the backend */
  readonly VITE_GREY_API_URL: string;
  /** Environment (development, production) */
  readonly VITE_ENVIRONMENT: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
