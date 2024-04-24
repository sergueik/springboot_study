'use strict';
var multipleUploadForm = document.querySelector('#multipleUploadForm');
var multipleFileUploadInput = document.querySelector('#multipleFileUploadInput');
var multipleFileUploadError = document.querySelector('#multipleFileUploadError');
var multipleFileUploadSuccess = document.querySelector('#multipleFileUploadSuccess');

// the <input type="file" miltiple> creates a FileList
// the file upload back end typically expects a FormData
function uploadMultipleFiles(files) {
  var formData = new FormData();
  // NOTE: syntax error would lead to console output being lost
  for (let file of files) {
    formData.append('files', file);
  }
  var xhr = new XMLHttpRequest();
  // uncomment for cgi testing
  // var uploadUrl = 'http://192.168.99.100:9090/cgi-bin/upload_multiple_files.cgi';
  var uploadUrl = '/uploadMultipleFiles';
  xhr.open('POST', uploadUrl);
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
  
  // console.dir(document.querySelector('#debug'));
  // var debug = (/true/).test(document.querySelector('#debug').textContent);
  var debug = new RegExp('true', 'i').test(document.querySelector('#debug').textContent);
  
  console.log('debug: '+ debug);
  var inputElement = document.querySelector('#input');
  if (debug) { 
    inputElement.classList.remove('hidden');
  } else {
    inputElement.classList.remove('hidden');
    inputElement.classList.add('hidden');
  }  

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
