package com.greylegacy.dao;

import com.greylegacy.domain.Adjuster;
import java.util.List;

public interface AdjusterDao extends GenericDao<Adjuster, Long> {
    Adjuster findByAdjusterCode(String adjusterCode);
    List<Adjuster> findAvailableAdjusters();
    List<Adjuster> findBySpecialization(String specialization);
}
