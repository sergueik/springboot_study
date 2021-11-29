package example.model;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "contact")
public class Contact {
	@Id
	@Column(name = "con_id")
	private int id;

	@Column(name = "con_civil")
	private String civil;

	@Column(name = "con_nom")
	private String nom;

	@Column(name = "con_prenom")
	private String prenom;

	@Column(name = "con_tel")
	private String tel;

	@Column(name = "con_email")
	private String email;

	@Column(name = "con_fax")
	private String fax;

	@Column(name = "con_adr1")
	private String addr1;

	@Column(name = "con_adr2")
	private String addr2;

	@Column(name = "con_adr3")
	private String addr3;

	@Column(name = "con_codpos")
	private String codpos;

	@Column(name = "con_ville")
	private String ville;

	@Column(name = "con_pays")
	private String pays;

	@Column(name = "con_natact")
	private String natact;

	@Column(name = "con_dialang")
	private String dialang;

	@Column(name = "con_ackmail")
	private String ackmail;

	@Column(name = "con_lastupdate")
	private Timestamp lastupdate;

	@Column(name = "con_lastupdate_id")
	private int lastupdateId;

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getCivil() {
		return civil;
	}

	public void setCivil(String civil) {
		this.civil = civil;
	}

	public String getNom() {
		return nom;
	}

	public void setNom(String nom) {
		this.nom = nom;
	}

	public String getPrenom() {
		return prenom;
	}

	public void setPrenom(String prenom) {
		this.prenom = prenom;
	}

	public String getTel() {
		return tel;
	}

	public void setTel(String tel) {
		this.tel = tel;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getFax() {
		return fax;
	}

	public void setFax(String fax) {
		this.fax = fax;
	}

	public String getAddr1() {
		return addr1;
	}

	public void setAddr1(String addr1) {
		this.addr1 = addr1;
	}

	public String getAddr2() {
		return addr2;
	}

	public void setAddr2(String addr2) {
		this.addr2 = addr2;
	}

	public String getAddr3() {
		return addr3;
	}

	public void setAddr3(String addr3) {
		this.addr3 = addr3;
	}

	public String getCodpos() {
		return codpos;
	}

	public void setCodpos(String codpos) {
		this.codpos = codpos;
	}

	public String getVille() {
		return ville;
	}

	public void setVille(String ville) {
		this.ville = ville;
	}

	public String getPays() {
		return pays;
	}

	public void setPays(String pays) {
		this.pays = pays;
	}

	public String getNatact() {
		return natact;
	}

	public void setNatact(String natact) {
		this.natact = natact;
	}

	public String getDialang() {
		return dialang;
	}

	public void setDialang(String dialang) {
		this.dialang = dialang;
	}

	public String getAckmail() {
		return ackmail;
	}

	public void setAckmail(String ackmail) {
		this.ackmail = ackmail;
	}

	public Timestamp getLastupdate() {
		return lastupdate;
	}

	public void setLastupdate(Timestamp lastupdate) {
		this.lastupdate = lastupdate;
	}

	public int getLastupdateId() {
		return lastupdateId;
	}

	public void setLastupdateId(int lastupdateId) {
		this.lastupdateId = lastupdateId;
	}

	@Override
	public String toString() {
		return "Contact [id=" + id + ", civil=" + civil + ", nom=" + nom
				+ ", prenom=" + prenom + ", tel=" + tel + ", email=" + email + ", fax="
				+ fax + ", addr1=" + addr1 + ", addr2=" + addr2 + ", addr3=" + addr3
				+ ", codpos=" + codpos + ", ville=" + ville + ", pays=" + pays
				+ ", natact=" + natact + ", dialang=" + dialang + ", ackmail=" + ackmail
				+ ", lastupdate=" + lastupdate + ", lastupdateId=" + lastupdateId + "]";
	}

}
