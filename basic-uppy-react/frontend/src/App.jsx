import { useState } from 'react'

import Uppy from '@uppy/core'
// import { Dashboard } from '@uppy/react'
// import Dashboard from '@uppy/react/lib/Dashboard'
import Dashboard from '@uppy/dashboard'
import XHRUpload from '@uppy/xhr-upload'

// import '@uppy/core/dist/style.min.css'
// import '@uppy/dashboard/dist/style.min.css'
import '@uppy/core/css/style.min.css'
import '@uppy/dashboard/css/style.min.css'

function App() {

  const [uppy] = useState(() => {

    return new Uppy({

      restrictions: {
        maxNumberOfFiles: 1
      }

    }).use(XHRUpload, {

      endpoint: '/uploadMultipleFiles',

      fieldName: 'files',

      formData: true,

      bundle: true
    })
  })

  return (
    <div style={{ padding: '20px' }}>

      <h2>Multipart Upload Demo</h2>

      <Dashboard
        uppy={uppy}
        proudlyDisplayPoweredByUppy={false}
      />

    </div>
  )
}

export default App

