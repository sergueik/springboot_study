package com.deb.qbe.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.deb.qbe.model.Customers;

@Repository
public interface CustomerRepository extends JpaRepository<Customers, Long>{

}
