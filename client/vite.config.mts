
import { defineConfig } from 'vite';
import { resolve } from 'path';

import vuejsPlugin from '@vitejs/plugin-vue';

export default defineConfig(({ command, mode, ssrBuild }) => {
  let config = {
    plugins: [
      vuejsPlugin(),
    ],
    define: {
      'process': { 'env': {} },
      'vite': (command === 'serve'),
    },
    build: {
      // legacy/win81: Electron 22 = Chromium 108. Vite's default baseline is
      // close to this already; pinning it stops a future Vite major from
      // silently emitting syntax the legacy renderer cannot parse.
      target: ['chrome108'],
      rollupOptions: {
        plugins: [

        ],
        input: {
          main: resolve(__dirname, 'index.html'),
          analysisui: resolve(__dirname, 'analysisui.html'),
          resultsview: resolve(__dirname, 'resultsview.html'),
        },
      }
    },
    server: {
      allowedHosts: ['vite'],
      watch: {
        usePolling: true,
      },
    },
  }

  if (command != 'build')
    // rollup mangles things with this
    config['define']['global'] = 'globalThis';

  return config;
});
