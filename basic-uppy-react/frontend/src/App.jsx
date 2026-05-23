import { useEffect } from 'react'

import Uppy from '@uppy/core'
import Dashboard from '@uppy/dashboard'
import XHRUpload from '@uppy/xhr-upload'

import '@uppy/core/css/style.min.css'
import '@uppy/dashboard/css/style.min.css'

export default function App() {

  useEffect(() => {

    const uppy = new Uppy({
      restrictions: {
        maxNumberOfFiles: 1
      }
    })

    uppy.use(Dashboard, {
      inline: true,
      target: '#uppy',
      proudlyDisplayPoweredByUppy: false
    })

    uppy.use(XHRUpload, {
      endpoint: '/uploadMultipleFiles',
      fieldName: 'files',
      formData: true,
      bundle: true,
      chunking: {
        enabled: true,
        size: 5 * 1024 * 1024
      },
      responseType: 'json'
    })

    return () => uppy.destroy()

  }, [])

  return (
    <div style={{ padding: 20 }}>
      <h2>Upload Demo</h2>
      <div id="uppy" />
    </div>
  )
}
