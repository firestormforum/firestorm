import {
  accept,
  base,
  body,
  createFetch,
  createStack,
  method,
  params,
  parse,
} from 'http-client'

// FIXME: set this in config
const basePath = 'http://localhost:4000/api/v1/'

const commonStack = createStack(
  base(basePath),
  accept('application/json'),
  parse('json', 'jsonData'),
)

const UploadSignature = {
  create: (filename, mimetype) => {
    const payload = {
      upload: {
        filename,
        mimetype
      }
    }
    const fetch = createFetch(
      commonStack,
      method('POST'),
      body(JSON.stringify(payload), 'application/json')
    )

    return fetch('/upload_signature')
  }
}

const Api = {
  UploadSignature
}

export default Api
