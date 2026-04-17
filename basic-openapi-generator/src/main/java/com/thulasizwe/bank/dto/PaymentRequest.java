package com.thulasizwe.bank.dto;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.UUID;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * PaymentRequest
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0")
public class PaymentRequest {

  private Double amount;

  private String currency;

  private UUID sourceAccountId;

  private UUID destinationAccountId;

  private String reference;

  public PaymentRequest() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public PaymentRequest(Double amount, String currency, UUID sourceAccountId, UUID destinationAccountId) {
    this.amount = amount;
    this.currency = currency;
    this.sourceAccountId = sourceAccountId;
    this.destinationAccountId = destinationAccountId;
  }

  public PaymentRequest amount(Double amount) {
    this.amount = amount;
    return this;
  }

  /**
   * Get amount
   * minimum: 0.01
   * @return amount
   */
  @NotNull
  @JsonProperty("amount")
  public Double getAmount() {
    return amount;
  }

  public void setAmount(Double amount) {
    this.amount = amount;
  }

  public PaymentRequest currency(String currency) {
    this.currency = currency;
    return this;
  }

  /**
   * Get currency
   * @return currency
   */
  @NotNull
  @JsonProperty("currency")
  public String getCurrency() {
    return currency;
  }

  public void setCurrency(String currency) {
    this.currency = currency;
  }

  public PaymentRequest sourceAccountId(UUID sourceAccountId) {
    this.sourceAccountId = sourceAccountId;
    return this;
  }

  /**
   * Get sourceAccountId
   * @return sourceAccountId
   */
  @NotNull
  @JsonProperty("sourceAccountId")
  public UUID getSourceAccountId() {
    return sourceAccountId;
  }

  public void setSourceAccountId(UUID sourceAccountId) {
    this.sourceAccountId = sourceAccountId;
  }

  public PaymentRequest destinationAccountId(UUID destinationAccountId) {
    this.destinationAccountId = destinationAccountId;
    return this;
  }

  /**
   * Get destinationAccountId
   * @return destinationAccountId
   */
  @NotNull
  @JsonProperty("destinationAccountId")
  public UUID getDestinationAccountId() {
    return destinationAccountId;
  }

  public void setDestinationAccountId(UUID destinationAccountId) {
    this.destinationAccountId = destinationAccountId;
  }

  public PaymentRequest reference(String reference) {
    this.reference = reference;
    return this;
  }

  /**
   * Get reference
   * @return reference
   */
  
  @JsonProperty("reference")
  public String getReference() {
    return reference;
  }

  public void setReference(String reference) {
    this.reference = reference;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PaymentRequest paymentRequest = (PaymentRequest) o;
    return Objects.equals(this.amount, paymentRequest.amount) &&
        Objects.equals(this.currency, paymentRequest.currency) &&
        Objects.equals(this.sourceAccountId, paymentRequest.sourceAccountId) &&
        Objects.equals(this.destinationAccountId, paymentRequest.destinationAccountId) &&
        Objects.equals(this.reference, paymentRequest.reference);
  }

  @Override
  public int hashCode() {
    return Objects.hash(amount, currency, sourceAccountId, destinationAccountId, reference);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PaymentRequest {\n");
    sb.append("    amount: ").append(toIndentedString(amount)).append("\n");
    sb.append("    currency: ").append(toIndentedString(currency)).append("\n");
    sb.append("    sourceAccountId: ").append(toIndentedString(sourceAccountId)).append("\n");
    sb.append("    destinationAccountId: ").append(toIndentedString(destinationAccountId)).append("\n");
    sb.append("    reference: ").append(toIndentedString(reference)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

