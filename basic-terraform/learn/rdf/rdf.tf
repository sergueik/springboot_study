provider "aws" {
  region     = "us-east-2"
  access_key = "ACCESS_KEY"
  secret_key = "SECRET_KEY"
}
/*
NOTE: will fail with status 400:

InvalidParameterCombination: RDS does not support creating a DB instance with the following combination: 
DBInstanceClass=db.t2.micro, 
Engine=mysql, 
EngineVersion=5.7.44, 
LicenseModel=general-public-license.
For supported combinations of instance class and database engine version, see the documentation.
*/
resource "aws_db_instance" "default" {
  allocated_storage    = 8
  db_name              = "mydb"
  engine               = "mysql"
  engine_version       = "5.7"
  instance_class       = "db.t2.micro"
  username             = "foo"
  password             = "foobarbaz"
  parameter_group_name = "default.mysql8.0"
  skip_final_snapshot  = true
}
