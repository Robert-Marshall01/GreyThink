package com.greylegacy.dao;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Example;
import org.hibernate.criterion.Projections;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;
import java.util.List;

/**
 * Abstract Hibernate DAO implementation using SessionFactory.
 * Demonstrates legacy Hibernate session management patterns.
 * 
 * Uses getCurrentSession() which relies on Spring-managed transactions
 * via the OpenSessionInViewFilter or @Transactional boundaries.
 */
@SuppressWarnings("unchecked")
public abstract class AbstractHibernateDao<T, PK extends Serializable> implements GenericDao<T, PK> {

    protected final Logger log = LoggerFactory.getLogger(getClass());

    private final Class<T> persistentClass;

    @Autowired
    private SessionFactory sessionFactory;

    protected AbstractHibernateDao(Class<T> persistentClass) {
        this.persistentClass = persistentClass;
    }

    protected Session getCurrentSession() {
        return sessionFactory.getCurrentSession();
    }

    protected Class<T> getPersistentClass() {
        return persistentClass;
    }

    @Override
    public T findById(PK id) {
        log.debug("Finding {} by id: {}", persistentClass.getSimpleName(), id);
        return (T) getCurrentSession().get(persistentClass, id);
    }

    @Override
    public List<T> findAll() {
        log.debug("Finding all {}", persistentClass.getSimpleName());
        return getCurrentSession()
                .createCriteria(persistentClass)
                .setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY)
                .list();
    }

    @Override
    public List<T> findByExample(T example) {
        log.debug("Finding {} by example", persistentClass.getSimpleName());
        return getCurrentSession()
                .createCriteria(persistentClass)
                .add(Example.create(example).excludeZeroes())
                .list();
    }

    @Override
    public T save(T entity) {
        log.debug("Saving new {}", persistentClass.getSimpleName());
        getCurrentSession().save(entity);
        return entity;
    }

    @Override
    public T update(T entity) {
        log.debug("Updating {}", persistentClass.getSimpleName());
        getCurrentSession().update(entity);
        return entity;
    }

    @Override
    public T saveOrUpdate(T entity) {
        log.debug("SaveOrUpdate {}", persistentClass.getSimpleName());
        getCurrentSession().saveOrUpdate(entity);
        return entity;
    }

    @Override
    public void delete(T entity) {
        log.debug("Deleting {}", persistentClass.getSimpleName());
        getCurrentSession().delete(entity);
    }

    @Override
    public void deleteById(PK id) {
        T entity = findById(id);
        if (entity != null) {
            delete(entity);
        }
    }

    @Override
    public void flush() {
        getCurrentSession().flush();
    }

    @Override
    public void clear() {
        getCurrentSession().clear();
    }

    @Override
    public long count() {
        return (Long) getCurrentSession()
                .createCriteria(persistentClass)
                .setProjection(Projections.rowCount())
                .uniqueResult();
    }

    /**
     * Execute a named query with parameters.
     */
    protected List<T> findByNamedQuery(String queryName, Object... params) {
        org.hibernate.Query query = getCurrentSession().getNamedQuery(queryName);
        for (int i = 0; i < params.length; i += 2) {
            query.setParameter((String) params[i], params[i + 1]);
        }
        return query.list();
    }
}
