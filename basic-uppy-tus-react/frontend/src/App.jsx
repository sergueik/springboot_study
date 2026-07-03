import { useEffect, useRef } from 'react'

import Uppy from '@uppy/core'
import Dashboard from '@uppy/dashboard'
import Tus from '@uppy/tus'
import { sha256 } from 'js-sha256';

import '@uppy/core/css/style.min.css'
import '@uppy/dashboard/css/style.min.css'
import uploadConfig from './uploadConfig';

const config = {
  TUS_ENDPOINT: window.APP_CONFIG?.TUS_ENDPOINT ?? uploadConfig.TUS_ENDPOINT,
  UPPY_SHOW_PROGRESS_DETAILS:
    window.APP_CONFIG?.UPPY_SHOW_PROGRESS_DETAILS ??
    uploadConfig.UPPY_SHOW_PROGRESS_DETAILS,

  TUS_CHUNK_SIZE: (() => {
    const v =
      window.APP_CONFIG?.TUS_CHUNK_SIZE ??
      uploadConfig.TUS_CHUNK_SIZE;

    const n = parseInt(v, 10);
    return Number.isNaN(n) ? undefined : n;
  })(),

  TUS_RETRY_DELAYS:
    window.APP_CONFIG?.TUS_RETRY_DELAYS ??
    uploadConfig.TUS_RETRY_DELAYS,
};

// NOTE: does not scale
async function calculateHash(file) {
  const buffer = await file.data.arrayBuffer();
  return sha256(buffer);
}

// NOTE: on HTTP, browser hosted crypto.subtle is unavailable by design

export default function App() {
  const uppyRef = useRef(null);

  const pauseUploads = () => {
    uppyRef.current?.pauseAll();
  };

  const resumeUploads = () => {
    uppyRef.current?.resumeAll();
  };

  useEffect(() => {
    if (!config) {
      console.log('config undefined');
      return;
    } else {
      console.dir(config); 
    }
    //  guard the effect
    const uppy = new Uppy({ autoProceed: false })

    uppyRef.current = uppy;

    uppy.use(Dashboard, {
      inline: true,
      target: '#uppy',
      proudlyDisplayPoweredByUppy: false,
      showProgressDetails: config.UPPY_SHOW_PROGRESS_DETAILS,
      hidePauseResumeButtons: true,
    });

    uppy.setMeta({
      filetype: 'application/octet-stream',
      filename: 'example.bin',
    });

    uppy.use(Tus, {
      endpoint: config.TUS_ENDPOINT,
      retryDelays: config.TUS_RETRY_DELAYS,
      ...(config.TUS_CHUNK_SIZE && {
        chunkSize: config.TUS_CHUNK_SIZE,
      }),
    });

    let cached;

    uppy.on('file-added', (file) => {
      cached = file;
      console.dir(cached);
    });

    uppy.on('upload-success', async (file, response) => {
      const uploadUrl = response?.uploadURL || response?.url;
      const uploadId = uploadUrl?.split('/').pop();
      const res = await fetch ('/api/uploads/finalize',{
        method: 'POST',
        headers: {
          'Content-Type':'application/json'
        },
        body: JSON.stringify({uploadId})
      });
      const data = await res.json().catch(() => ({}));
      console.log('finalize: ', res.status, data );
      // NOTE: there is a TUS protocol extension called Checksum that allows the client to send a checksum per chunk.

     const rawFile = cached;

     if (!rawFile) {
       console.error('No file data available for hashing');
       return;
     }

     const hash = await calculateHash(rawFile);

     console.log('client side: ', hash);

	    // const hash = 'hash';	
      const res2 =  await fetch('/api/uploads/validate', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ uploadId, hash })
      });

      const data2 = await res2.json();

      console.log('verify: ', res2.status, data2);
    });

    return () => uppy.destroy();
  }, []);

  return (
    <div style={{ padding: 20 }}>
      <h2>Upload Demo</h2>
      <button onClick={pauseUploads}>
        Pause
      </button>

      <button onClick={resumeUploads}>
        Resume
      </button>	
      <div id="uppy" />
    </div>
  );
}
