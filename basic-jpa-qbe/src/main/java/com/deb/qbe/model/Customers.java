package com.deb.qbe.model;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Entity
@Table(name = "customers")
@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
@ToString
@Builder
public class Customers implements Serializable{

	private static final long serialVersionUID = 34131213L;

	@Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

	@Column(name = "first_name", nullable = false)
    private String firstName;
	
	@Column(name = "last_name")
    private String lastName;
	
	@Column(name = "balance")
	private Long   walletBalance;
	
	
}
