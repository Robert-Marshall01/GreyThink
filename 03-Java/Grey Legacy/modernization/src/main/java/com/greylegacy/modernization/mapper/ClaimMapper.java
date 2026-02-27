package com.greylegacy.modernization.mapper;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimPayment;
import com.greylegacy.modernization.dto.ClaimDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.mapstruct.factory.Mappers;

import java.util.List;

/**
 * MapStruct mapper for Claim entity → ClaimDto conversion.
 *
 * <p>Replaces the hand-written conversion code that was scattered across
 * legacy Struts Actions and Spring MVC controllers. MapStruct generates
 * the implementation at compile time, providing type-safe, null-safe mapping
 * with zero runtime reflection overhead.</p>
 *
 * <h3>Migration from Legacy Pattern:</h3>
 * <pre>
 * // BEFORE (hand-written in every controller/action):
 * Map&lt;String, Object&gt; json = new HashMap&lt;&gt;();
 * json.put("claimNumber", claim.getClaimNumber());
 * json.put("policyNumber", claim.getPolicy().getPolicyNumber());
 * json.put("status", claim.getStatus().name());
 * // ... 20 more lines of manual mapping
 *
 * // AFTER (MapStruct):
 * ClaimDto dto = ClaimMapper.INSTANCE.toDto(claim);
 * </pre>
 */
@Mapper(componentModel = "spring")
public interface ClaimMapper {

    ClaimMapper INSTANCE = Mappers.getMapper(ClaimMapper.class);

    @Mapping(source = "policy.policyNumber", target = "policyNumber")
    @Mapping(source = "status", target = "status", qualifiedByName = "enumToString")
    @Mapping(source = "claimType", target = "claimType", qualifiedByName = "enumToString")
    @Mapping(source = "fraudRiskLevel", target = "fraudRiskLevel", qualifiedByName = "enumToString")
    @Mapping(source = "payments", target = "payments")
    ClaimDto toDto(Claim claim);

    List<ClaimDto> toDtoList(List<Claim> claims);

    @Mapping(source = "paymentStatus", target = "status", qualifiedByName = "enumToString")
    @Mapping(source = "paymentMethod", target = "method", qualifiedByName = "enumToString")
    ClaimDto.PaymentDto paymentToDto(ClaimPayment payment);

    List<ClaimDto.PaymentDto> paymentsToDto(List<ClaimPayment> payments);

    /**
     * Named qualifier for converting any enum to its String name.
     * Handles null enums gracefully.
     */
    @Named("enumToString")
    default String enumToString(Enum<?> value) {
        return value != null ? value.name() : null;
    }
}
