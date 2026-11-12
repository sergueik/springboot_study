import js from '@eslint/js';
import globals from 'globals';

/**
 * Deliberately small: the type checker (tsconfig.json, `npm run typecheck`)
 * catches type problems, so ESLint only has to catch the things it cannot -
 * unused code, accidental globals, sloppy equality.
 */
export default [
    js.configs.recommended,
    {
        files: ['**/*.js'],
        languageOptions: {
            ecmaVersion: 2022,
            sourceType: 'script',
            globals: {
                ...globals.browser,
                ...globals.node,
                JSZip: 'readonly',
                pako: 'readonly'
                // The browser sources are plain scripts sharing one global
                // scope; each file declares what it consumes with a
                // /* global ... */ comment, so a missing <script> tag in
                // index.html shows up as a lint error.
            }
        },
        rules: {
            eqeqeq: ['error', 'always', { null: 'ignore' }],
            'no-var': 'error',
            'prefer-const': 'error',
            // The browser sources define these for the other page scripts;
            // ESLint cannot see across files, so name the public surface here.
            'no-unused-vars': ['error', {
                argsIgnorePattern: '^_',
                varsIgnorePattern: '^(SceneModel|SceneLayout|SvgTransform|SvgStyleResolver|SvgParser|DrawioParser|VsdxBuilder|DrawioBuilder|OdgBuilder)$'
            }],
            'no-implicit-globals': 'error',
            'no-console': 'off'
        }
    },
    {
        // These files are CommonJS modules, not page scripts
        files: ['build.js', 'test.js', 'test-golden.js', 'test-vsdx.js', 'test-behavior.js', 'test-export.js', 'tools/*.js'],
        languageOptions: { sourceType: 'commonjs' }
    },
    {
        ignores: ['dist/**', 'node_modules/**', 'test-golden/**']
    }
];
