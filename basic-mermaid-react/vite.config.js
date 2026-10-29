import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
const mermaidPackage = require('mermaid/package.json');

export default defineConfig({
  plugins: [react()],
  resolve: {
    dedupe: [
      '@mermaid'
      ]
  },
  define: {
    __MERMAID_VERSION__: JSON.stringify(mermaidPackage.version)
  }
})
