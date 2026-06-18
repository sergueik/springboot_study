import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  resolve: {
    dedupe: [
      '@uppy/core',
      '@uppy/dashboard',
      '@uppy/xhr-upload',
      '@uppy/react'
    ]
  }
})
