// Third-party libraries the page loads as plain <script> tags, and which
// tools/load.js mirrors onto globalThis for Node. Declared here so the type
// checker knows about them without introducing a bundler or module system.

declare global {
    var JSZip: any;
    var pako: any;
}

export {};
