package com.greylegacy.dao;

import java.io.Serializable;
import java.util.List;

/**
 * Generic DAO interface providing standard CRUD operations.
 * All entity-specific DAOs extend this interface.
 */
public interface GenericDao<T, PK extends Serializable> {
    T findById(PK id);
    List<T> findAll();
    List<T> findByExample(T example);
    T save(T entity);
    T update(T entity);
    T saveOrUpdate(T entity);
    void delete(T entity);
    void deleteById(PK id);
    void flush();
    void clear();
    long count();
}
