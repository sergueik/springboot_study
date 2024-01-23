'use strict';
var multipleUploadForm = document.querySelector('#multipleUploadForm');
var multipleFileUploadInput = document.querySelector('#multipleFileUploadInput');
var multipleFileUploadError = document.querySelector('#multipleFileUploadError');
var multipleFileUploadSuccess = document.querySelector('#multipleFileUploadSuccess');

// the <input type="file" miltiple> creates a FileList
// the file upload back end typically expects a FormData
function uploadMultipleFiles(files) {
  var formData = new FormData();
  console.log('files: ' + files);
  for (var index = 0; index < files.length; index++) {
    formData.append('files', files[index]);
  }
  console.log('formData files: ');
  console.dir(formData.getAll('files'));
  // NOTE: syntax error would lead to console output being lost
  console.log('formData entries: ');
  console.dir(formData.entries());
  try {
    var entries = formData.entries();
    for (let entry of entries) {
      console.log('formData entry: ');
      console.dir(entry);
      // Array(2)
      // "files"
      // File { "name": "...", "lastModified":  1705973287311, "lastModifiedDate": "Mon ...", "size": 7, "type": "text/plain", "webkitRelativePath": 2 }
    }
  } catch (exception) {
    console.log('exception :' + exception);
  }
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/uploadMultipleFiles');
  xhr.onload = function() {
    // console.log(xhr.responseText);
    if (xhr.status == 200) {
      multipleFileUploadError.style.display = 'none';
      var content = xhr.responseText;
      multipleFileUploadSuccess.innerHTML = '<pre>' + content + '</pre>';
      multipleFileUploadSuccess.style.display = 'block';
    } else {
      multipleFileUploadSuccess.style.display = 'none';
      multipleFileUploadError.innerHTML = (response && response.message) || 'Some Error Occurred';
    }
    document.getElementById('multipleUploadForm').reset();
  }
  xhr.send(formData);
}
multipleUploadForm.addEventListener('submit', function(event) {
  var files = multipleFileUploadInput.files;
  if (files.length === 0) {
    multipleFileUploadError.innerHTML = 'Please select at least one file';
    multipleFileUploadError.style.display = 'block';
  }
  uploadMultipleFiles(files);
  event.preventDefault();
}, true);
window.addEventListener('load', function(event) {
  var xhr = new XMLHttpRequest();
  var multipleFileUploadSuccess = document.querySelector('#multipleFileUploadSuccess');
  xhr.open('GET', '/listFiles');
  var content = '<p>Reading Files</p>';
  xhr.onload = function() {
    console.log(xhr.responseText);
    if (xhr.status == 200) {
      multipleFileUploadError.style.display = 'none';
      var content = xhr.responseText;
      multipleFileUploadSuccess.innerHTML = '<pre>' + content + '</pre>';
      multipleFileUploadSuccess.style.display = 'block';
    } else {
      multipleFileUploadSuccess.style.display = 'none';
      multipleFileUploadError.innerHTML = (response && response.message) || 'Some Error Occurred';
    }
  }
  xhr.send();
  multipleFileUploadSuccess.innerHTML = content;
});
