import { Injectable } from '@angular/core';
import{Http, Response, Headers, RequestOptions} from '@angular/http';
import{Observable}   from 'rxjs/Observable';
import 'rxjs/add/operator/map';
import 'rxjs/add/operator/catch';
import 'rxjs/add/observable/throw';
import{User}  from '../user';
import { HttpClient, HttpHeaders, HttpClientModule } from '@angular/common/http';



@Injectable()
export class UserService {
  
  private baseUrl:string='http://103.108.140.153:8084/api';
  //old private headers = new Headers({'Content-Type':'application/json'});
  private headers = new Headers({ 
    'Content-Type':'application/json',
    'Access-Control-Allow-Origin':'*',
    'Authorization':'authkey',
    'userid':'1'
  });
  private options = new RequestOptions({headers:this.headers});
  private user = new User();
  
  constructor(private _http:Http) { }

  getUsers(){

    return this._http.get(this.baseUrl+'/users',this.options).map((response:Response)=>response.json())
      .catch(this.errorHandler);
  }
  getUser(id:Number){

    return this._http.get(this.baseUrl+'/user/'+id,this.options).map((response:Response)=>response.json())
      .catch(this.errorHandler);
  }
  deleteUser(id:Number){

    return this._http.delete(this.baseUrl+'/user/'+id,this.options).map((response:Response)=>response.json())
      .catch(this.errorHandler);
  }


  createUser(user:User){

    return this._http.post(this.baseUrl+'/user',JSON.stringify(user),  this.options).map((response:Response)=>response.json())
      .catch(this.errorHandler);
  }
   
   updateUser(user:User){

    return this._http.put(this.baseUrl+'/user',JSON.stringify(user),  this.options).map((response:Response)=>response.json())
      .catch(this.errorHandler);
  }
  
  errorHandler(error:Response){

     return Observable.throw(error||"SERVER ERROR");
  }

   setter(user:User){
     this.user=user;
   }

  getter(){
    return this.user;
  }
}
