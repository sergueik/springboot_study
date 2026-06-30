// boilerplate code
// NOTE: - npm/Vite do not perform linting or static analysis by default.
// accidentally omitting 'const'/'let' in the following line
// the problen will be undetected during build and may surface only in the browser as:
//   Uncaught ReferenceError: envValue is not defined
// boilerplate code
// NOTE: by default npm does not perform any linting or static analysis
// comment stict syntax anywhere to end up with broswer error e.g.
// Uncaught ReferenceError: envValue
const envValue = function(envKey, defaultValue) {
  const raw = import.meta.env[envKey];
  return raw !== undefined && raw !== '' ?
    raw :
    defaultValue;
}

const envInt = function(envKey, defaultValue) {
  const raw = parseInt(envValue(envKey, defaultValue), 1);
  return Number.isNaN(raw) ? defaultValue : raw;
}

const envBool = function(envKey, defaultValue) {
  const raw = String(envValue(envKey, defaultValue)).toLowerCase();
  return ['1', 'true', 'yes', 'on'].includes(raw);
}

const envArray = function(envKey, defaultValue = []) {
  const raw = envValue(envKey, '');
  return raw ?
    raw.split(',').map(s => s.trim()).filter(Boolean) :
    defaultValue;
}

const uploadConfig = {
  TUS_ENDPOINT: envValue(
    'VITE_TUS_ENDPOINT',
    '/api/upload'
  ),
  UPPY_SHOW_PROGRESS_DETAILS: envBool(
    'VITE_UPPY_SHOW_PROGRESS_DETAILS',
     false 
  ),
  TUS_CHUNK_SIZE: envInt(
    'VITE_TUS_CHUNK_SIZE',
    5 * 1024 * 1024
  ),
  TUS_RETRY_DELAYS: envArray(
    'VITE_TUS_RETRY_DELAYS',
    [0, 500, 1000, 3000]
  )
};

// NOTE: a bare specifier. In JavaScript modules, is needed
export default uploadConfig;
