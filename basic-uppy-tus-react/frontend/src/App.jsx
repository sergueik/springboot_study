import { useEffect } from 'react'

import Uppy from '@uppy/core'
import Dashboard from '@uppy/dashboard'
import Tus from '@uppy/tus'

import '@uppy/core/css/style.min.css'
import '@uppy/dashboard/css/style.min.css'

export default function App() {

  useEffect(() => {

    const uppy = new Uppy({ autoProceed: false })

    uppy.use(Dashboard, {
      inline: true,
      target: '#uppy',
      proudlyDisplayPoweredByUppy: false
    })

    uppy.setMeta({
      filetype: 'application/octet-stream',
      filename: 'example.bin'
    })

    uppy.use(Tus, {
      endpoint: '/api/upload',
      chunkSize: 5 * 1024 * 1024,
      retryDelays: [0,500,1000,3000]
    })

    uppy.on('upload-success', async (file, response) => {
      const uploadUrl = response?.uploadURL||response?.url;
      const uploadId = uploadUrl?.split('/').pop();
      const res = await fetch ('/api/uploads/finalize',{
        method: 'POST',
        headers: {
          'Content-Type':'application/json'
        },
        body: JSON.stringify({uploadId})
      });
      const data = await res.json().catch(()=>({}));
      console.log('finalize: ', res.status, data );
    });

    return () => uppy.destroy()
  }, [])

  return (
    <div style={{ padding: 20 }}>
      <h2>Upload Demo</h2>
      <div id="uppy" />
    </div>
  )
}
