package com.greylegacy.modernization.mapper;

import com.greylegacy.domain.Policy;
import com.greylegacy.modernization.dto.PolicyDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.mapstruct.factory.Mappers;

import java.util.List;

/**
 * MapStruct mapper for Policy entity → PolicyDto conversion.
 *
 * <p>Key features demonstrated:</p>
 * <ul>
 *   <li>@Mapping for field name mismatches and nested property access</li>
 *   <li>Custom @Named qualifiers for enum-to-string conversion</li>
 *   <li>Collection mapping (claims size → claimCount)</li>
 *   <li>Exclusion of sensitive fields (holderSsn not mapped)</li>
 * </ul>
 */
@Mapper(componentModel = "spring")
public interface PolicyMapper {

    PolicyMapper INSTANCE = Mappers.getMapper(PolicyMapper.class);

    @Mapping(source = "status", target = "status", qualifiedByName = "enumToString")
    @Mapping(source = "policyType", target = "policyType", qualifiedByName = "enumToString")
    @Mapping(source = "claims", target = "claimCount", qualifiedByName = "claimListToCount")
    // holderSsn intentionally excluded — never expose SSN in API responses
    PolicyDto toDto(Policy policy);

    List<PolicyDto> toDtoList(List<Policy> policies);

    @Named("enumToString")
    default String enumToString(Enum<?> value) {
        return value != null ? value.name() : null;
    }

    @Named("claimListToCount")
    default int claimListToCount(java.util.List<?> claims) {
        return claims != null ? claims.size() : 0;
    }
}
