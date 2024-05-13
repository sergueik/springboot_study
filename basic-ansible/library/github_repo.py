#!/usr/bin/python
# based on: https://jruels.github.io/adv-ansible/labs/gh_module/

from ansible.module_utils.basic import *
import requests
global api_url
api_url = 'https://api.github.com'
def github_repo_present(data):

  api_key = data['github_auth_key']

  del data['state']
  del data['github_auth_key']

  headers = { 'Authorization': 'token {}' . format(api_key) }
  url = "{}{}" . format(api_url, '/user/repos')
  result = requests.post(url, json.dumps(data), headers=headers)

  if result.status_code == 201:
    return False, True, result.json()
  if result.status_code == 422:
    return False, False, result.json()

  # default: something went wrong
  meta = {"status": result.status_code, 'response': result.json()}
  return True, False, meta

def github_repo_absent(data = None):
  headers = { 'Authorization': 'token {}'.format(api_key) }
  url = '{}/repos/{}/{}'.format(api_url, data['username'], data['name'])
  result = requests.delete(url, headers = headers)

  if result.status_code == 204:
    return False, True, {'status': 'SUCCESS'}
  if result.status_code == 404:
    result = {'status': result.status_code, 'data': result.json()}
    return False, False, result
  else:
    result = {'status': result.status_code, 'data': result.json()}
    return True, False, result

def main():

  # define the parameter schema
  fields = {
    'github_auth_key': {'required': True, 'type': 'str'},
    'username': {'required': True, 'type': 'str'},
    'name': {'required': True, 'type': 'str'},
    'description': {'required': False, 'type': 'str'},
    'private': {'default': False, 'type': 'bool'},
    'has_issues': {'default': True, 'type': 'bool'},
    'has_wiki': {'default': True, 'type': 'bool'},
    'has_downloads': {'default': True, 'type': 'bool'},
    'state': {
      'default': 'present',
      'choices': ['present', 'absent'],
      'type': 'str'
    },
  }
  choice_map = { 'present': github_repo_present, 'absent': github_repo_absent, }

  module = AnsibleModule(argument_spec=fields)
  is_error, has_changed, result = choice_map.get( module.params['state'])(module.params)


  if not is_error:
    module.exit_json(changed=has_changed, meta=result)
  else:
    module.fail_json(msg='Error deleting repo', meta=result)


if __name__ == '__main__':
    main()

